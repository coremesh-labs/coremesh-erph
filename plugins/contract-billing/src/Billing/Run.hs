-- | Ein Buchungslauf: Daten lesen (Datenströme aus contract, realestate,
-- ledger), planen ("Billing.Plan"), je Beleg in einer Transaktion buchen:
--
--   1. JournalDraft.create      – Buchungskopf der Vorerfassung
--   2. JournalDraftItem.create  – je Position
--   3. JournalDraft.post        – Beleg im Universal Journal
--   4. contract_billing__posting – gebuchte Fälligkeiten vermerken
--
-- Alle vier Schritte laufen in derselben Transaktion (der Ledger nimmt an ihr
-- teil); scheitert einer, ist nichts gebucht und der Lauf meldet den Fehler für
-- diesen Beleg und macht mit dem nächsten weiter.
module Billing.Run
  ( RunRequest (..)
  , runBilling
  , readAll
  , currencyDecimals
  ) where

import Control.Exception (try)
import Control.Monad (forM, forM_, when)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Billing.Data
import Billing.Plan
import Billing.Schedule (Due (..))
import Billing.Store
import CoreMesh.Plugin

data RunRequest = RunRequest
  { rrCompany :: Text
  , rrTo :: Day
  , rrContract :: Maybe Text -- ^ nur dieser Vertrag
  , rrSimulate :: Bool -- ^ nur planen, nichts buchen
  }

-- | Liest (object, list) als Datenstrom vollständig.
readAll :: Call -> Text -> [(Text, Value)] -> IO [Record]
readAll call obj params = do
  cols <- newIORef []
  rows <- newIORef []
  _ <-
    dispatchRead call obj "list" (Object (KM.fromList [(K.fromText k, v) | (k, v) <- params])) $
      RowWriter
        { writeHeader = writeIORef cols . headerColumns
        , writeRows = \rs -> modifyIORef' rows (reverse rs <>)
        }
  records <$> readIORef cols <*> (reverse <$> readIORef rows)

-- | Nachkommastellen der Währung aus dem Hauptbuch (Standard 2).
currencyDecimals :: Call -> Text -> IO Int
currencyDecimals call cur = do
  r <- try (dispatch call "Currency" "get" (object ["id" .= cur]))
  pure $ case r of
    Right (Object o) | Just (Number n) <- KM.lookup "decimals" o -> floor n
    Left (_ :: PluginError) -> 2
    _ -> 2

newId :: IO Text
newId = TE.decodeUtf8 . convertToBase Base16 <$> (getRandomBytes 12 :: IO ByteString)

now :: IO Text
now = T.pack . iso8601Show <$> getCurrentTime

runBilling :: Call -> RunRequest -> IO Value
runBilling call RunRequest {..} = do
  let cc = [("company_code", String rrCompany)]
      history = ("includeHistory", Bool True)
      only :: (a -> Text) -> [a] -> [a]
      only f = maybe id (\c -> filter ((== c) . f)) rrContract
  types <- map contractType <$> readAll call "ContractType" cc
  contracts <- only cId . concatMap (maybe [] pure . contract) <$> readAll call "Contract" (history : cc)
  condRecs <- readAll call "ContractCondition" (history : cc)
  let parsed = [(text "contract_id" r, condition r) | r <- condRecs]
      conditions = only kContract [k | (_, Right k) <- parsed]
      badConditions = [(c, "Kondition nicht lesbar: " <> e) | (c, Left e) <- parsed, maybe True (== c) rrContract]
  objects <- only oContract . concatMap (maybe [] pure . contractObject) <$> readAll call "ContractObject" (history : cc)
  measurements <-
    if any kPerUnit conditions
      then concatMap (maybe [] pure . measurement) <$> readAll call "Measurement" (history : cc)
      else pure []
  accounts <- map account <$> readAll call "GLAccountCompany" [("company_code_id", String rrCompany)]
  status <- fieldStatus <$> readAll call "FieldStatus" []
  posted <- postedKeys call rrCompany rrContract
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
  decimals <- Map.fromList <$> forM (nub (map (cCurrency . dContract) docs)) (\c -> (c,) <$> currencyDecimals call c)
  let dec c = Map.findWithDefault 2 c decimals
      itemCount = sum (map (length . dEntries) docs)
  if rrSimulate
    then
      pure $
        object
          [ "message" .= simulationText (length docs) itemCount (length msgs0)
          , "documents" .= length docs
          , "items" .= itemCount
          , "planned" .= map (docJson dec) docs
          , "messages" .= map msgJson msgs0
          ]
    else do
      runId <- newId
      started <- now
      insertRun call runId rrCompany rrTo rrContract started
      results <- forM docs $ \d -> do
        r <- try (inTx call database (\tx -> postDoc tx runId (dec (cCurrency (dContract d))) d))
        pure (d, r)
      let ok = [(d, n) | (d, Right n) <- results]
          failed = [(cId (dContract d), "Beleg " <> formatDay (dDate d) <> ": " <> errText e) | (d, Left e) <- results]
          msgs = msgs0 <> failed
          items = sum [length (dEntries d) | (d, _) <- ok]
          runStatus
            | null docs && null msgs = "EMPTY"
            | null ok && not (null msgs) = "FAILED"
            | not (null msgs) = "PARTIAL"
            | otherwise = "DONE"
          message = runText (length ok) items (length msgs) rrTo
      finished <- now
      finishRun call runId runStatus (length ok) items (length msgs) message (jsonText (map msgJson msgs)) finished
      when (not (null failed)) $
        logMessage call LogWarn "Buchungslauf mit Fehlern" [("run", runId), ("errors", T.pack (show (length msgs)))]
      pure $
        object
          [ "id" .= runId
          , "_id" .= runId
          , "message" .= message
          , "status" .= (runStatus :: Text)
          , "documents" .= length ok
          , "items" .= items
          , "errors" .= length msgs
          , "posted" .= [object ["contract_id" .= cId (dContract d), "due_date" .= formatDay (dDate d), "document_number" .= n] | (d, n) <- ok]
          , "messages" .= map msgJson msgs
          ]
  where
    errText (PluginError _ m) = m

-- | Bucht einen Beleg über die Vorerfassung; liefert die Belegnummer.
postDoc :: Call -> Text -> Int -> Doc -> IO Text
postDoc tx runId decimals Doc {..} = do
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
  forM_ dLines $ \Line {..} ->
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
  res <- dispatch tx "JournalDraft" "post" (object ["id" .= draftId])
  docId <- field "id" res
  docNo <- field "document_number" res
  stampNow <- now
  forM_ dEntries (insertPosting tx (cCompany c) runId stampNow draftId docId docNo (cCurrency c))
  pure docNo
  where
    field k v = case v of
      Object o | Just (String s) <- KM.lookup (K.fromText k) o -> pure s
      _ -> pluginError Internal ("Antwort des Ledgers ohne " <> k)

docJson :: (Text -> Int) -> Doc -> Value
docJson dec Doc {..} =
  object
    [ "contract_id" .= cId dContract
    , "due_date" .= formatDay dDate
    , "document_type" .= dDocumentType
    , "lines"
        .= [ object ["account" .= lAccount, "side" .= lSide, "amount" .= formatAmount (dec (cCurrency dContract)) lAmount, "text" .= lText]
           | Line {..} <- dLines
           ]
    , "periods" .= [formatDay (dueFrom (eDue e)) <> "–" <> formatDay (dueTo (eDue e)) | e <- dEntries]
    ]

msgJson :: (Text, Text) -> Value
msgJson (c, m) = object ["contract_id" .= c, "message" .= m]

jsonText :: J.ToJSON a => a -> Text
jsonText = TL.toStrict . TLE.decodeUtf8 . J.encode

simulationText :: Int -> Int -> Int -> Text
simulationText docs items errs =
  "Vorschau: " <> tshow docs <> " Belege mit " <> tshow items <> " Fälligkeiten" <> problems errs

runText :: Int -> Int -> Int -> Day -> Text
runText docs items errs to =
  tshow docs <> " Belege mit " <> tshow items <> " Fälligkeiten gebucht bis " <> formatDay to <> problems errs

problems :: Int -> Text
problems 0 = ""
problems n = "; " <> tshow n <> " Meldungen"

tshow :: Show a => a -> Text
tshow = T.pack . show
