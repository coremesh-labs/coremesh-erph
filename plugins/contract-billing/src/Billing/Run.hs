-- | Ein Buchungslauf – ohne SQL, nur über Objects und Actions:
--
--   1. Stammdaten aus dem Cache ("Billing.Cache"), die schon vermerkten
--      Fälligkeiten frisch aus ContractPosting (Datenstrom).
--   2. Nur Verträge, die der Benutzer sehen darf (Account.Check: Contract.read
--      je Buchungskreis und Vertragsart).
--   3. Planen ("Billing.Plan").
--   4. Je Beleg in einer Transaktion: JournalDraft.create, je Position
--      JournalDraftItem.create, JournalDraft.simulate (nicht buchbar = nichts
--      angelegt), bei „automatisch buchen“ der Vertragsart JournalDraft.post,
--      zuletzt ContractPostingService.record (Fälligkeiten vermerken).
--
-- Die Transaktion öffnet das Plugin über die Host-API (inTx); Ledger und
-- Vertragsmodul nehmen an ihr teil. Scheitert ein Schritt, ist für diesen
-- Beleg nichts angelegt; der Lauf meldet es und macht weiter.
module Billing.Run
  ( RunRequest (..)
  , runBilling
  , currencyDecimals
  ) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)

import Billing.Cache
import Billing.Data
import Billing.Plan
import Billing.Schedule (Due (..))
import CoreMesh.Plugin

data RunRequest = RunRequest
  { rrCompany :: Text
  , rrTo :: Day
  , rrContract :: Maybe Text -- ^ nur dieser Vertrag
  , rrSimulate :: Bool -- ^ nur planen, nichts anlegen
  , rrRunId :: Maybe Text -- ^ Lauf im Vertragsmodul (ContractPostingRun)
  }

-- | Ergebnis eines Belegs.
data Outcome = Outcome
  { oDraft :: Text
  , oDocument :: Maybe Text -- ^ Belegnummer, wenn gebucht
  }

database :: Text
database = "main"

-- | Nachkommastellen der Währung aus dem Hauptbuch (Standard 2).
currencyDecimals :: Call -> Text -> IO Int
currencyDecimals call cur = do
  r <- try (dispatch call "Currency" "get" (object ["id" .= cur]))
  pure $ case r of
    Right (Object o) | Just (Number n) <- KM.lookup "decimals" o -> floor n
    Left (_ :: PluginError) -> 2
    _ -> 2

runBilling :: Cache -> Call -> RunRequest -> IO Value
runBilling cache call RunRequest {..} = do
  sys <- systemCall call (callTenantId call)
  let get = slice cache sys rrCompany
      only :: (a -> Text) -> [a] -> [a]
      only f = maybe id (\c -> filter ((== c) . f)) rrContract
  types <- map contractType <$> get Types
  allContracts <- only cId . concatMap (maybe [] pure . contract) <$> get Contracts
  contracts <- visibleContracts call allContracts
  let ids = Set.fromList (map cId contracts)
      mine :: (a -> Text) -> [a] -> [a]
      mine f = filter ((`Set.member` ids) . f)
  condRecs <- get Conditions
  let parsed = [(text "contract_id" r, condition r) | r <- condRecs]
      conditions = mine kContract [k | (_, Right k) <- parsed]
      badConditions = [(c, "Kondition nicht lesbar: " <> e) | (c, Left e) <- parsed, c `Set.member` ids]
  objects <- mine oContract . concatMap (maybe [] pure . contractObject) <$> get Objects
  measurements <-
    if any kPerUnit conditions
      then concatMap (maybe [] pure . measurement) <$> get Measurements
      else pure []
  accounts <- map account <$> get Accounts
  status <- fieldStatus <$> get FieldStatuses
  posted <- postedKeys sys rrCompany
  let input =
        Input
          { inTypes = Map.fromList [(ctCode t, t) | t <- types]
          , inContracts = contracts
          , inConditions = conditions
          , inObjects = objects
          , inMeasurements = measurements
          , inAccounts = Map.fromList [(aNumber a, a) | a <- accounts]
          , inFieldStatus = status
          , inPosted = posted
          }
      (docs, planMsgs) = plan rrTo input
      msgs0 = badConditions <> planMsgs
      autoPost d = maybe False ctAutoPost (Map.lookup (cType (dContract d)) (inTypes input))
  decimals <- Map.fromList <$> forM (nub (map (cCurrency . dContract) docs)) (\c -> (c,) <$> currencyDecimals call c)
  let dec c = Map.findWithDefault 2 c decimals
  if rrSimulate
    then
      pure $
        object
          [ "message" .= ("Vorschau: " <> tshow (length docs) <> " Belege mit " <> tshow (sum (map (length . dEntries) docs)) <> " Fälligkeiten" <> problems (length msgs0))
          , "documents" .= length docs
          , "items" .= sum (map (length . dEntries) docs)
          , "planned" .= map (docJson dec autoPost) docs
          , "messages" .= map msgJson msgs0
          ]
    else do
      results <- forM docs $ \d -> do
        r <- try (inTx call database (\tx -> postDoc tx rrRunId (dec (cCurrency (dContract d))) (autoPost d) d))
        pure (d, r)
      let ok = [(d, o) | (d, Right o) <- results]
          failed = [(cId (dContract d), "Beleg " <> formatDay (dDate d) <> ": " <> errText e) | (d, Left e) <- results]
          msgs = msgs0 <> failed
          items = sum [length (dEntries d) | (d, _) <- ok]
          drafts = length [() | (_, o) <- ok, oDocument o == Nothing]
          runStatus :: Text
          runStatus
            | null docs && null msgs = "EMPTY"
            | null ok && not (null msgs) = "FAILED"
            | not (null msgs) = "PARTIAL"
            | otherwise = "DONE"
          message =
            tshow (length ok) <> " Belege mit " <> tshow items <> " Fälligkeiten bis " <> formatDay rrTo
              <> " (" <> tshow (length ok - drafts) <> " gebucht, " <> tshow drafts <> " vorerfasst)" <> problems (length msgs)
      if null failed then pure () else logMessage call LogWarn "Sollstellung mit Meldungen" [("errors", tshow (length msgs))]
      pure $
        object
          [ "status" .= runStatus
          , "message" .= message
          , "documents" .= length ok
          , "drafts" .= drafts
          , "items" .= items
          , "errors" .= length msgs
          , "results"
              .= [ object ["contract_id" .= cId (dContract d), "due_date" .= formatDay (dDate d), "draft_id" .= oDraft o, "document_number" .= oDocument o]
                 | (d, o) <- ok
                 ]
          , "messages" .= map msgJson msgs
          ]
  where
    errText (PluginError _ m) = m

-- | Nur Verträge, die der Benutzer lesen darf (Systemanfragen: alle).
visibleContracts :: Call -> [Contract] -> IO [Contract]
visibleContracts call cs
  | T.null (callUserId call) = pure cs
  | otherwise = do
      let combos = nub [(cCompany c, cType c) | c <- cs]
      allowed <- forM combos $ \(cc, t) -> do
        r <-
          dispatch call "Account" "Check" $
            object ["object" .= ("Contract" :: Text), "action" .= ("read" :: Text), "company_code" .= cc, "attrs" .= object ["contract_type" .= t]]
        pure ((cc, t), r == object ["allowed" .= True])
      let ok = Map.fromList allowed
      pure [c | c <- cs, Map.findWithDefault False (cCompany c, cType c) ok]

-- | Bereits vermerkte Fälligkeiten (vorerfasst oder gebucht) – immer frisch.
postedKeys :: Call -> Text -> IO (Set.Set Key)
postedKeys sys cc = do
  rs <- readAll sys "ContractPosting" [("company_code", String cc)]
  pure . Set.fromList $
    [ (text "contract_id" r, text "condition_type" r, text "object_id" r, d)
    | r <- rs
    , Just d <- [day "period_from" r]
    ]

-- | Vorerfassung anlegen, füllen, prüfen, ggf. buchen, Fälligkeiten vermerken.
postDoc :: Call -> Maybe Text -> Int -> Bool -> Doc -> IO Outcome
postDoc tx runId decimals autoPost Doc {..} = do
  let c = dContract
      stamp = T.filter (/= '-') (T.pack (iso8601Show dDate))
  draft <-
    dispatch tx "JournalDraft" "create" $
      object
        [ "data"
            .= object
              [ "company_code_id" .= cCompany c
              , "document_type" .= dDocumentType
              , "posting_date" .= iso8601Show dDate
              , "document_date" .= iso8601Show dDate
              , "currency" .= cCurrency c
              , "header_text" .= ("Sollstellung " <> cId c <> " " <> formatDay dDate)
              , "reference" .= (cId c <> "/" <> stamp)
              ]
        ]
  draftId <- field "id" draft
  mapM_ (addLine draftId) dLines
  _ <- dispatch tx "JournalDraft" "simulate" (object ["id" .= draftId])
  (docId, docNo) <-
    if autoPost
      then do
        res <- dispatch tx "JournalDraft" "post" (object ["id" .= draftId])
        (,) <$> (Just <$> field "id" res) <*> (Just <$> field "document_number" res)
      else pure (Nothing, Nothing)
  _ <-
    dispatch tx "ContractPostingService" "record" $
      object
        [ "company_code" .= cCompany c
        , "run_id" .= runId
        , "draft_id" .= draftId
        , "status" .= (if autoPost then "POSTED" else "DRAFT" :: Text)
        , "document_id" .= docId
        , "document_number" .= docNo
        , "currency" .= cCurrency c
        , "entries" .= map entryJson dEntries
        ]
  pure (Outcome draftId docNo)
  where
    addLine draftId Line {..} =
      dispatch tx "JournalDraftItem" "create" $
        object
          [ "data"
              .= object
                ( [ "draft_id" .= draftId
                  , "account_number" .= lAccount
                  , "shkzg" .= lSide
                  , "amount" .= formatAmount decimals lAmount
                  , "item_text" .= lText
                  ]
                    <> [K.fromText k .= v | (k, v) <- lFields]
                )
          ]
    field k v = case v of
      Object o | Just (String s) <- KM.lookup (K.fromText k) o -> pure s
      _ -> pluginError Internal ("Antwort ohne " <> k)

entryJson :: Entry -> Value
entryJson e =
  let k = eCondition e
      d = eDue e
   in object
        [ "contract_id" .= kContract k
        , "condition_type" .= kType k
        , "object_id" .= kObject k
        , "period_from" .= iso8601Show (dueFrom d)
        , "period_to" .= iso8601Show (dueTo d)
        , "due_date" .= iso8601Show (dueDate d)
        , "amount" .= dueAmount d
        , "account_number" .= eAccount e
        , "rent_object_id" .= eRentObject e
        ]

docJson :: (Text -> Int) -> (Doc -> Bool) -> Doc -> Value
docJson dec autoPost d@Doc {..} =
  object
    [ "contract_id" .= cId dContract
    , "due_date" .= formatDay dDate
    , "document_type" .= dDocumentType
    , "auto_post" .= autoPost d
    , "lines"
        .= [ object ["account" .= lAccount, "side" .= lSide, "amount" .= formatAmount (dec (cCurrency dContract)) lAmount, "text" .= lText]
           | Line {..} <- dLines
           ]
    , "periods" .= [formatDay (dueFrom (eDue e)) <> "–" <> formatDay (dueTo (eDue e)) | e <- dEntries]
    ]

msgJson :: (Text, Text) -> Value
msgJson (c, m) = object ["contract_id" .= c, "message" .= m]

problems :: Int -> Text
problems 0 = ""
problems n = "; " <> tshow n <> " Meldungen"

tshow :: Show a => a -> Text
tshow = T.pack . show
