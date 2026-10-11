-- | Schnittstelle des Plugins – ohne eigene Daten, ohne SQL, nichts wird gespeichert.
--
-- | Object.Action | Payload | Ergebnis |
-- |---|---|---|
-- | ProfitLoss.compute | company_code, rent_object_id?, period_from, period_to (auch unter "data") | message, table, download (CSV) |
-- | ProfitLoss.formState | FormStateRequest | Vorbelegung des Zeitraums (laufendes Jahr bis heute) |
--
-- Gelesen wird mit den Rechten des Benutzers: Sachkonten (GLAccountCompany,
-- GLAccount) und Einzelposten (JournalEntryItem) des Hauptbuchs.
module ProfitLoss.Api
  ( profitLossPlugin
  , parseSettings
  ) where

import Control.Exception (try)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.IORef
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import CoreMesh.Plugin
import ProfitLoss.Meta (describe, moduleName)
import ProfitLoss.Report

profitLossPlugin :: IORef Settings -> Plugin
profitLossPlugin ref =
  (defaultPlugin moduleName "0.1.0")
    { pluginDescription = "GuV-Rechnung: Gewinn und Verlust je Buchungskreis, Mietobjekt und Zeitraum (Einzelposten des Hauptbuchs)"
    , pluginCapabilities =
        [Capability "ProfitLoss" ["compute", "formState"] [] "GuV rechnen: Bildschirm und CSV, nichts wird gespeichert"]
    , pluginDescribe = Just describe
    , pluginConfigure = \_ settings -> writeIORef ref (parseSettings settings)
    , pluginHandle = \call req -> readIORef ref >>= \s -> handler s call req
    }

-- | settings: ledger (Standard 0L), sections: [{title, side: INCOME|EXPENSE, account_types: [...]}].
parseSettings :: KM.KeyMap Value -> Settings
parseSettings o =
  Settings
    { setLedger = case KM.lookup "ledger" o of
        Just (String l) | not (T.null (T.strip l)) -> T.strip l
        _ -> setLedger defaultSettings
    , setSections = case KM.lookup "sections" o of
        Just (Array xs) | ss@(_ : _) <- mapMaybe section (toList xs) -> ss
        _ -> setSections defaultSettings
    }
  where
    section = \case
      Object s
        | Just (String t) <- KM.lookup "title" s
        , Just (Array ts) <- KM.lookup "account_types" s ->
            Just
              Section
                { secTitle = t
                , secSide = if KM.lookup "side" s == Just (String "INCOME") then Income else Expense
                , secTypes = [x | String x <- toList ts]
                }
      _ -> Nothing

type Record = KM.KeyMap Value

-- | Felder aus Payload und "data".
params :: Value -> Record
params = \case
  Object o -> case KM.lookup "data" o of
    Just (Object d) -> d <> o
    _ -> o
  _ -> mempty

text :: Text -> Record -> Text
text k r = case KM.lookup (K.fromText k) r of
  Just (String s) -> T.strip s
  Just (Number n) -> maybe (T.pack (show n)) (T.pack . show) (toBoundedInteger n :: Maybe Int)
  _ -> ""

int :: Text -> Record -> Maybe Integer
int k r = case KM.lookup (K.fromText k) r of
  Just (Number n) -> toInteger <$> (toBoundedInteger n :: Maybe Int)
  Just (String s) | [(n, "")] <- reads (T.unpack s) -> Just n
  _ -> Nothing

day :: Text -> Record -> Maybe Day
day k r = iso8601ParseM (T.unpack (T.take 10 (text k r)))

handler :: Settings -> Call -> Request -> IO Response
handler settings call req = case (reqObject req, reqAction req) of
  ("ProfitLoss", "formState") -> do
    today <- utctDay <$> getCurrentTime
    let (y, _, _) = toGregorian today
        values = case KM.lookup "values" p of
          Just (Object v) -> v
          _ -> mempty
        preset k v = [K.fromText k .= object ["value" .= v] | T.null (text k values)]
    pure . response $
      object
        [ "fields" .= object (preset "period_from" (iso8601Show (fromGregorian y 1 1)) <> preset "period_to" (iso8601Show today))
        , "message" .= ("Gewinn und Verlust aus den gebuchten Einzelposten (Buchungsdatum im Zeitraum). Nichts wird gespeichert." :: Text)
        ]
  ("ProfitLoss", "compute") -> do
    cc <- required "company_code" "Buchungskreis"
    from <- date "period_from" "Zeitraum von"
    to <- date "period_to" "Zeitraum bis"
    if from > to then pluginError InvalidArgument "Zeitraum: „von“ liegt nach „bis“" else pure ()
    let obj = text "rent_object_id" p
    response <$> compute settings call cc obj from to
  (o, a) -> pluginError Unimplemented (o <> "." <> a)
  where
    p = params (reqPayload req)
    required k label = case text k p of
      "" -> pluginError InvalidArgument (label <> " ist Pflicht")
      v -> pure v
    date k label = do
      v <- required k label
      maybe (pluginError InvalidArgument (label <> ": Datum JJJJ-MM-TT erwartet, nicht " <> v)) pure (iso8601ParseM (T.unpack (T.take 10 v)))

compute :: Settings -> Call -> Text -> Text -> Day -> Day -> IO Value
compute settings call cc obj from to = do
  company <- readAll call "GLAccountCompany" [("company_code_id", String cc)]
  if null company then pluginError InvalidArgument ("Buchungskreis " <> cc <> ": keine Sachkonten (oder keine Berechtigung)") else pure ()
  masters <-
    concat <$> mapM (\ch -> readAll call "GLAccount" [("chart_of_accounts_id", String ch)]) (nub (filter (not . T.null) (map (text "chart_of_accounts_id") company)))
  let master = Map.fromList [(text "account_number" m, m) | m <- masters]
      accounts =
        [ Account n (if T.null (text "account_name" c) then maybe "" (text "name") m else text "account_name" c) (maybe "" (text "account_type") m)
        | c <- company
        , let n = text "account_number" c
              m = Map.lookup n master
        ]
  raw <-
    readAll call "JournalEntryItem" $
      [("company_code_id", String cc), ("ledger", String (setLedger settings))] <> [("rent_object_id", String obj) | not (T.null obj)]
  let items =
        [ Item (text "account_number" r) d a
        | r <- raw
        , T.null (text "ledger" r) || text "ledger" r == setLedger settings
        , T.null obj || text "rent_object_id" r == obj
        , Just d <- [day "posting_date" r]
        , Just a <- [int "amount_local_curr" r]
        ]
      currency = fromMaybe "EUR" (lookup True [(True, c) | r <- raw, let c = text "local_currency" r, not (T.null c)])
  dec <- currencyDecimals call currency
  let rep = computeReport settings accounts from to items
      rows = tableRows dec rep
      period = formatDay from <> "–" <> formatDay to
      header =
        [("GuV-Rechnung", ""), ("Buchungskreis", cc), ("Mietobjekt", if T.null obj then "alle" else obj), ("Zeitraum", period), ("Währung", currency)]
      file = "GuV_" <> cc <> (if T.null obj then "" else "_" <> safe obj) <> "_" <> T.pack (iso8601Show from) <> "_" <> T.pack (iso8601Show to) <> ".csv"
      content = TE.decodeUtf8 (convertToBase Base64 (TE.encodeUtf8 ("\xFEFF" <> csv header tableColumns rows)) :: ByteString)
      message =
        "GuV Buchungskreis " <> cc <> (if T.null obj then "" else ", Mietobjekt " <> obj) <> ", " <> period <> ": "
          <> T.pack (show (rpItems rep)) <> " Einzelposten, Ergebnis "
          <> formatAmountDe dec (rpResult rep) <> " " <> currency
          <> (if rpResult rep >= 0 then " (Gewinn)" else " (Verlust)")
  pure $
    object
      [ "message" .= message
      , "result" .= rpResult rep
      , "table" .= object ["columns" .= tableColumns, "rows" .= rows]
      , "download" .= object ["content" .= content, "content_type" .= ("text/csv;charset=utf-8" :: Text), "file_name" .= file]
      ]
  where
    safe = T.map (\c -> if c `elem` ("/\\:*?\"<>| " :: String) then '-' else c)

currencyDecimals :: Call -> Text -> IO Int
currencyDecimals call cur = do
  r <- try (dispatch call "Currency" "get" (object ["id" .= cur]))
  pure $ case r of
    Right (Object o) | Just (Number n) <- KM.lookup "decimals" o -> floor n
    Left (_ :: PluginError) -> 2
    _ -> 2

-- | Alle Datensätze eines Objects (Datenstrom list) mit den Rechten des Benutzers.
readAll :: Call -> Text -> [(Text, Value)] -> IO [Record]
readAll call obj ps = do
  cols <- newIORef []
  rows <- newIORef []
  _ <-
    dispatchRead call obj "list" (Object (KM.fromList [(K.fromText k, v) | (k, v) <- ps])) $
      RowWriter
        { writeHeader = writeIORef cols . headerColumns
        , writeRows = \rs -> modifyIORef' rows (reverse rs <>)
        }
  cs <- readIORef cols
  map (KM.fromList . zip (map K.fromText cs)) . reverse <$> readIORef rows
