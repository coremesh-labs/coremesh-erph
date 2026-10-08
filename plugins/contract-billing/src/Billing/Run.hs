-- | Ein Buchungslauf – ohne SQL, nur über Objects und Actions:
--
--   1. Stammdaten aus dem Cache ("Billing.Cache"), die schon vermerkten
--      Sollstellungen (ContractPosting) und die Buchungskreisdaten der Partner
--      (PartnerCompanyCode, Abstimmkonto) frisch als Datenstrom.
--   2. Nur Verträge, die der Benutzer sehen darf (Account.Check: Contract.read
--      je Buchungskreis und Vertragsart).
--   3. Planen ("Billing.Plan"): offene Fälligkeiten und Nachberechnung schon
--      vermerkter Perioden (Buchungsdatum: Tag des Laufs, höchstens der Stichtag).
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
  , loanPlan
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
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)

import Billing.Cache
import Billing.Data
import Billing.Loan
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
  loanRecs <- get Loans
  let parsedLoans = [(text "contract_id" r, loanTerms r) | r <- loanRecs]
      loans = mine lnContract [l | (_, Right l) <- parsedLoans]
      badLoans = [(c, "Darlehenskondition nicht lesbar: " <> e) | (c, Left e) <- parsedLoans, c `Set.member` ids]
  payments <- mine lpContract . concatMap (maybe [] pure . loanPayment) <$> get LoanPayments
  accounts <- map account <$> get Accounts
  status <- fieldStatus <$> get FieldStatuses
  postings <- postedOf sys rrCompany
  -- Buchungskreisdaten der Partner (Abstimmkonto, Buchungssperre) – wie die Vermerke immer frisch
  partnerAccounts <- map partnerAccount <$> readAll sys "PartnerCompanyCode" [("company_code", String rrCompany)]
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let input =
        Input
          { inTypes = Map.fromList [(ctCode t, t) | t <- types]
          , inContracts = contracts
          , inConditions = conditions
          , inObjects = objects
          , inMeasurements = measurements
          , inAccounts = Map.fromList [(aNumber a, a) | a <- accounts]
          , inFieldStatus = status
          , inLoans = loans
          , inLoanPayments = payments
          , inPartnerAccounts = Map.fromList [((paPartner a, paRole a), a) | a <- partnerAccounts]
          , inPosted = postings
          , inCorrectionDate = min today rrTo
          }
      (docs, planMsgs) = plan rrTo input
      msgs0 = badConditions <> badLoans <> planMsgs
      autoPost d = maybe False ctAutoPost (Map.lookup (cType (dContract d)) (inTypes input))
      corrections ds = sum (map docCorrections ds)
  decimals <- Map.fromList <$> forM (nub (map (cCurrency . dContract) docs)) (\c -> (c,) <$> currencyDecimals call c)
  let dec c = Map.findWithDefault 2 c decimals
  if rrSimulate
    then
      pure $
        object
          [ "message"
              .= ( "Vorschau: " <> tshow (length docs) <> " Belege mit " <> tshow (sum (map (length . dEntries) docs)) <> " Fälligkeiten"
                    <> correctionText (corrections docs) <> problems (length msgs0)
                 )
          , "documents" .= length docs
          , "items" .= sum (map (length . dEntries) docs)
          , "corrections" .= corrections docs
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
          corrected = corrections (map fst ok)
          message =
            tshow (length ok) <> " Belege mit " <> tshow items <> " Fälligkeiten bis " <> formatDay rrTo
              <> correctionText corrected
              <> " (" <> tshow (length ok - drafts) <> " gebucht, " <> tshow drafts <> " vorerfasst)" <> problems (length msgs)
      if null failed then pure () else logMessage call LogWarn "Sollstellung mit Meldungen" [("errors", tshow (length msgs))]
      pure $
        object
          [ "status" .= runStatus
          , "message" .= message
          , "documents" .= length ok
          , "drafts" .= drafts
          , "items" .= items
          , "corrections" .= corrected
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

-- | Bereits vermerkte Sollstellungen (vorerfasst oder gebucht) – immer frisch.
postedOf :: Call -> Text -> IO [Posted]
postedOf sys cc = concatMap (maybe [] pure . posted) <$> readAll sys "ContractPosting" [("company_code", String cc)]

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
              , "header_text" .= (headerKind <> " " <> cId c <> " " <> formatDay dDate)
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
    headerKind :: Text
    headerKind
      | all ((== Correction) . eKind) dEntries = "Nachberechnung"
      | otherwise = "Sollstellung"
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
        , "kind" .= (if eKind e == Correction then "CORRECTION" else "ORIGINAL" :: Text)
        , "billing_period" .= iso8601Show (duePeriodFrom d)
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
    , "corrections" .= docCorrections d
    , "lines"
        .= [ object ["account" .= lAccount, "side" .= lSide, "amount" .= formatAmount (dec (cCurrency dContract)) lAmount, "text" .= lText]
           | Line {..} <- dLines
           ]
    , "periods"
        .= [ (if eKind e == Correction then "Nachberechnung " else "") <> kType (eCondition e) <> " "
              <> formatDay (dueFrom (eDue e)) <> "–" <> formatDay (dueTo (eDue e))
           | e <- dEntries
           ]
    ]

msgJson :: (Text, Text) -> Value
msgJson (c, m) = object ["contract_id" .= c, "message" .= m]

correctionText :: Int -> Text
correctionText 0 = ""
correctionText n = ", davon " <> tshow n <> " Nachberechnungen"

problems :: Int -> Text
problems 0 = ""
problems n = "; " <> tshow n <> " Meldungen"

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Tilgungsplan eines Darlehensvertrags (Aktion „Tilgungsplan“): alle Zeilen
-- bis zur Rückzahlung bzw. zum Vertragsende, als Tabelle.
loanPlan :: Cache -> Call -> Text -> Text -> IO Value
loanPlan cache call cc contractId = do
  sys <- systemCall call (callTenantId call)
  let get = slice cache sys cc
  terms <- concatMap (either (const []) pure . loanTerms) <$> get Loans
  payments <- concatMap (maybe [] pure . loanPayment) <$> get LoanPayments
  contracts <- concatMap (maybe [] pure . contract) <$> get Contracts
  let mineL = [l | l <- terms, lnContract l == contractId]
      c = [x | x <- contracts, cId x == contractId]
  case (mineL, c) of
    ([], _) -> pluginError NotFound ("Vertrag " <> contractId <> " hat keine Darlehenskonditionen")
    (_, []) -> pluginError NotFound ("Vertrag " <> contractId <> " gibt es im Buchungskreis " <> cc <> " nicht")
    (_, k : _) -> do
      dec <- currencyDecimals call (cCurrency k)
      let rows = schedule mineL [p | p <- payments, lpContract p == contractId] (cTo k) (fromGregorian 2199 12 31)
          amt = formatAmount dec
          kind r = case rKind r of
            Disbursement -> "Auszahlung" :: Text
            Regular -> "Rate"
            Special -> "Sondertilgung"
          regular = [r | r <- rows, rKind r == Regular]
          totalInterest = sum (map rInterest rows)
          lastDue = case reverse regular of
            r : _ -> formatDay (rDue r)
            [] -> "–"
      pure $
        object
          [ "message"
              .= ( "Tilgungsplan " <> contractId <> ": " <> tshow (length regular) <> " Raten bis " <> lastDue <> ", Zinsen gesamt "
                    <> amt totalInterest <> " " <> cCurrency k
                 )
          , "table"
              .= object
                [ "columns" .= (["Fällig", "Art", "Zinsperiode", "Zins", "Tilgung", "Rate", "Restschuld"] :: [Text])
                , "rows"
                    .= [ [ formatDay (rDue r), kind r
                         , if rKind r == Regular then formatDay (rFrom r) <> "–" <> formatDay (rTo r) else ""
                         , amt (rInterest r), amt (rPrincipal r), amt (rInterest r + if rKind r == Disbursement then 0 else rPrincipal r), amt (rBalance r)
                         ]
                       | r <- rows
                       ]
                ]
          ]
