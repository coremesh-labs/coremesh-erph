-- | Eigene Tabellen (Präfix contract_billing__): Buchungsläufe und gebuchte
-- Fälligkeiten. Die gebuchten Fälligkeiten sind die Wahrheit für „seit der
-- letzten Ausführung“: Gebucht wird alles, was bis zum Stichtag fällig und
-- hier noch nicht eingetragen ist. Der Eintrag entsteht in derselben
-- Transaktion wie der Beleg im Hauptbuch.
module Billing.Store
  ( schemaHCL
  , database
  , postedKeys
  , insertPosting
  , insertRun
  , finishRun
  , listRuns
  , getRun
  , listPostings
  ) where

import Data.Aeson (Value (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)

import Billing.Data
import Billing.Plan
import Billing.Schedule (Due (..))
import CoreMesh.Plugin

database :: Text
database = "main"

schemaHCL :: Text
schemaHCL =
  T.unlines
    [ "schema \"main\" {}"
    , ""
    , "# Buchungslauf: Stichtag, Ergebnis, Meldungen (JSON-Liste)."
    , "table \"contract_billing__run\" {"
    , "  schema = schema.main"
    , "  column \"id\"           { type = text }"
    , "  column \"company_code\" { type = text }"
    , "  column \"to_date\"      { type = text }"
    , "  column \"contract_id\"  {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  # RUNNING | DONE | PARTIAL (mit Fehlern) | FAILED | EMPTY (nichts fällig)"
    , "  column \"status\"       { type = text }"
    , "  column \"documents\" {"
    , "    type    = bigint"
    , "    default = 0"
    , "  }"
    , "  column \"items\" {"
    , "    type    = bigint"
    , "    default = 0"
    , "  }"
    , "  column \"errors\" {"
    , "    type    = bigint"
    , "    default = 0"
    , "  }"
    , "  column \"message\" {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  column \"messages\" {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  column \"started_at\"   { type = text }"
    , "  column \"started_by\" {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  column \"finished_at\" {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  primary_key { columns = [column.id] }"
    , "  index \"contract_billing__run_company\" { columns = [column.company_code, column.started_at] }"
    , "}"
    , ""
    , "# Gebuchte Fälligkeit: eine Kondition für einen Zeitraum, mit Beleg im Hauptbuch."
    , "table \"contract_billing__posting\" {"
    , "  schema = schema.main"
    , "  column \"company_code\"    { type = text }"
    , "  column \"contract_id\"     { type = text }"
    , "  column \"condition_type\"  { type = text }"
    , "  # leer = Kondition für den ganzen Vertrag"
    , "  column \"object_id\"       { type = text }"
    , "  column \"period_from\"     { type = text }"
    , "  column \"period_to\"       { type = text }"
    , "  column \"due_date\"        { type = text }"
    , "  # kleinste Einheit der Währung (Cent)"
    , "  column \"amount\"          { type = bigint }"
    , "  column \"currency\"        { type = text }"
    , "  column \"account_number\"  { type = text }"
    , "  column \"rent_object_id\" {"
    , "    type = text"
    , "    null = true"
    , "  }"
    , "  column \"draft_id\"        { type = text }"
    , "  column \"document_id\"     { type = text }"
    , "  column \"document_number\" { type = text }"
    , "  column \"run_id\"          { type = text }"
    , "  column \"posted_at\"       { type = text }"
    , "  primary_key { columns = [column.company_code, column.contract_id, column.condition_type, column.object_id, column.period_from] }"
    , "  index \"contract_billing__posting_run\" { columns = [column.run_id] }"
    , "  index \"contract_billing__posting_due\" { columns = [column.company_code, column.due_date] }"
    , "}"
    ]

dayText :: Day -> Value
dayText = String . T.pack . iso8601Show

-- | Bereits gebuchte Fälligkeiten des Buchungskreises (optional eines Vertrags).
postedKeys :: Call -> Text -> Maybe Text -> IO (Set Key)
postedKeys call cc contractId = do
  let (cond, args) = case contractId of
        Nothing -> ("", [])
        Just c -> (" AND contract_id = ?", [String c])
  r <-
    query call database
      ("SELECT contract_id, condition_type, object_id, period_from FROM contract_billing__posting WHERE company_code = ?" <> cond)
      (String cc : args)
  pure . Set.fromList $
    [ (text "contract_id" x, text "condition_type" x, text "object_id" x, d)
    | x <- records (qrColumns r) (qrRows r)
    , Just d <- [day "period_from" x]
    ]

-- | Vermerkt eine gebuchte Fälligkeit (in der Transaktion des Belegs).
insertPosting :: Call -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Entry -> IO ()
insertPosting call cc runId now draftId docId docNo currency e = do
  let d = eDue e
      (c, t, o, from) = entryKey e
  _ <-
    exec call database
      "INSERT INTO contract_billing__posting (company_code, contract_id, condition_type, object_id, period_from, period_to,\
      \ due_date, amount, currency, account_number, rent_object_id, draft_id, document_id, document_number, run_id, posted_at)\
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      [ String cc, String c, String t, String o, dayText from, dayText (dueTo d)
      , dayText (dueDate d), Number (fromInteger (dueAmount d)), String currency, String (eAccount e)
      , maybe Null String (eRentObject e), String draftId, String docId, String docNo, String runId, String now
      ]
  pure ()

insertRun :: Call -> Text -> Text -> Day -> Maybe Text -> Text -> IO ()
insertRun call runId cc to contractId now = do
  _ <-
    exec call database
      "INSERT INTO contract_billing__run (id, company_code, to_date, contract_id, status, started_at, started_by)\
      \ VALUES (?, ?, ?, ?, 'RUNNING', ?, ?)"
      [String runId, String cc, dayText to, maybe Null String contractId, String now, String (callUserId call)]
  pure ()

finishRun :: Call -> Text -> Text -> Int -> Int -> Int -> Text -> Text -> Text -> IO ()
finishRun call runId status docs items errs message messagesJson now = do
  _ <-
    exec call database
      "UPDATE contract_billing__run SET status = ?, documents = ?, items = ?, errors = ?, message = ?, messages = ?, finished_at = ?\
      \ WHERE id = ?"
      [ String status, Number (fromIntegral docs), Number (fromIntegral items), Number (fromIntegral errs)
      , String message, String messagesJson, String now, String runId
      ]
  pure ()

runColumns :: Text
runColumns = "id, company_code, to_date, contract_id, status, documents, items, errors, message, messages, started_at, started_by, finished_at"

-- | Läufe, neueste zuerst; companies leer = alle sichtbaren.
listRuns :: Call -> [Text] -> Maybe Text -> IO [Record]
listRuns call companies cc = do
  let (w, a) = companyFilter companies cc
  r <- query call database ("SELECT " <> runColumns <> " FROM contract_billing__run" <> w <> " ORDER BY started_at DESC") a
  pure (records (qrColumns r) (qrRows r))

getRun :: Call -> Text -> IO (Maybe Record)
getRun call runId = do
  r <- query call database ("SELECT " <> runColumns <> " FROM contract_billing__run WHERE id = ?") [String runId]
  pure (case records (qrColumns r) (qrRows r) of
    x : _ -> Just x
    [] -> Nothing)

-- | Gebuchte Fälligkeiten mit Filtern (company_code, contract_id, run_id).
listPostings :: Call -> [Text] -> [(Text, Text)] -> IO [Record]
listPostings call companies filters = do
  let (w, a) = companyFilter companies (lookup "company_code" filters)
      extra = [(f, v) | (f, v) <- filters, f `elem` ["contract_id", "run_id", "condition_type"], not (T.null v)]
      w' = w <> T.concat [(if T.null w && i == (0 :: Int) then " WHERE " else " AND ") <> f <> " = ?" | (i, (f, _)) <- zip [0 ..] extra]
  r <-
    query call database
      ( "SELECT company_code, contract_id, condition_type, object_id, period_from, period_to, due_date, amount, currency,\
        \ account_number, rent_object_id, draft_id, document_id, document_number, run_id, posted_at\
        \ FROM contract_billing__posting" <> w' <> " ORDER BY company_code, contract_id, due_date, condition_type"
      )
      (a <> map (String . snd) extra)
  pure (records (qrColumns r) (qrRows r))

-- | Sichtbare Buchungskreise (leer = alle) und optional einer davon.
companyFilter :: [Text] -> Maybe Text -> (Text, [Value])
companyFilter companies cc = case (companies, cc) of
  ([], Nothing) -> ("", [])
  ([], Just c) -> (" WHERE company_code = ?", [String c])
  (cs, Nothing) -> (" WHERE company_code IN (" <> T.intercalate ", " (map (const "?") cs) <> ")", map String cs)
  (cs, Just c)
    | c `elem` cs -> (" WHERE company_code = ?", [String c])
    | otherwise -> (" WHERE 1 = 0", [])
