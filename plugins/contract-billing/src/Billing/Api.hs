-- | Object ContractBilling: Schnittstelle des Plugins.
--
-- | Action | Payload | Ergebnis |
-- |---|---|---|
-- | run | company_code, to_date, contract_id?, simulate? | Lauf mit Meldungen (simulate: geplante Belege) |
-- | runs | company_code?, company_codes? | {items: Läufe, neueste zuerst} |
-- | getRun | id | Lauf |
-- | postings | company_code?, contract_id?, run_id?, company_codes? | {items: gebuchte Fälligkeiten}; auch als Datenstrom |
-- | getPosting | id (Buchungskreis|Vertrag|Konditionsart|Objekt|Beginn) | gebuchte Fälligkeit |
--
-- company_codes schränkt Listen auf die Buchungskreise ein, die der Aufrufer
-- sehen darf (die Oberfläche im Plugin contract setzt sie aus den Rechten).
-- Payloads dürfen die Felder auch unter "data" bzw. "query" tragen (Oberfläche).
module Billing.Api
  ( billingPlugin
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Network.HTTP.Types.URI (urlDecode, urlEncode)

import Billing.Data
import Billing.Plan (formatAmount)
import Billing.Run
import Billing.Store
import CoreMesh.Plugin

billingPlugin :: Plugin
billingPlugin =
  (defaultPlugin "contract-billing" "0.1.0")
    { pluginDescription = "Vertragsbuchung: Sollstellung der Verträge über die Vorerfassung des Hauptbuchs"
    , pluginCapabilities =
        [ Capability "ContractBilling" ["run", "runs", "getRun", "postings", "getPosting"] ["postings"]
            "Sollstellung der Verträge (Buchungsläufe, gebuchte Fälligkeiten)"
        ]
    , pluginSchema = Just schemaHCL
    , pluginHandle = handler
    , pluginRead = reader
    }

-- | Felder aus Payload, "data" und "query" zusammen.
params :: Value -> Record
params = \case
  Object o ->
    let sub k = case KM.lookup k o of
          Just (Object x) -> x
          _ -> mempty
     in sub "data" <> sub "query" <> o
  _ -> mempty

companies :: Record -> [Text]
companies p = case KM.lookup "company_codes" p of
  Just (Array xs) -> [s | String s <- V.toList xs]
  Just (String s) | not (T.null s) -> T.splitOn "," s
  _ -> []

handler :: Call -> Request -> IO Response
handler call req = case reqAction req of
  "run" -> do
    cc <- required "company_code" "Buchungskreis"
    toText <- required "to_date" "Buchen bis"
    to <- maybe (pluginError InvalidArgument ("Buchen bis: Datum JJJJ-MM-TT erwartet, nicht " <> toText)) pure (iso8601ParseM (T.unpack toText))
    response
      <$> runBilling call RunRequest {rrCompany = cc, rrTo = to, rrContract = textMaybe "contract_id" p, rrSimulate = bool "simulate" p}
  "runs" -> do
    rs <- listRuns call (companies p) (textMaybe "company_code" p)
    pure (response (object ["items" .= map runJson rs]))
  "getRun" -> do
    i <- required "id" "ID"
    getRun call i >>= maybe (pluginError NotFound ("Buchungslauf " <> i)) (pure . response . runJson)
  "postings" -> do
    rs <- listPostings call (companies p) (filters p)
    response . (\xs -> object ["items" .= xs]) <$> formatPostings call rs
  "getPosting" -> do
    i <- required "id" "ID"
    case map decode (T.splitOn "|" i) of
      [cc, c, t, o, from] -> do
        rs <- listPostings call [] [("company_code", cc), ("contract_id", c), ("condition_type", t)]
        case [r | r <- rs, text "object_id" r == o, text "period_from" r == from] of
          r : _ -> response . head' <$> formatPostings call [r]
          [] -> pluginError NotFound ("Fälligkeit " <> i)
      _ -> pluginError InvalidArgument ("ID " <> i <> ": Buchungskreis|Vertrag|Konditionsart|Objekt|Beginn erwartet")
  a -> pluginError Unimplemented ("ContractBilling." <> a)
  where
    p = params (reqPayload req)
    required k label = maybe (pluginError InvalidArgument (label <> " ist Pflicht")) pure (textMaybe k p)
    head' = \case
      x : _ -> x
      [] -> Null

filters :: Record -> [(Text, Text)]
filters p = [(k, v) | k <- ["company_code", "contract_id", "run_id", "condition_type"], Just v <- [textMaybe k p]]

-- | ContractBilling.postings als Datenstrom: Rohwerte (Betrag in Cent).
reader :: Call -> Request -> RowWriter -> IO ReadEnd
reader call req w = case reqAction req of
  "postings" -> do
    let p = params (reqPayload req)
    rs <- listPostings call (companies p) (filters p)
    let cols =
          [ "company_code", "contract_id", "condition_type", "object_id", "period_from", "period_to", "due_date"
          , "amount", "currency", "account_number", "rent_object_id", "document_number", "run_id"
          ]
    writeHeader w (ReadHeader cols (Map.fromList [("object", "ContractBilling")]))
    writeRows w [[maybe Null id (KM.lookup (K.fromText c) r) | c <- cols] | r <- rs]
    pure readEnd
  a -> pluginError Unimplemented ("ContractBilling." <> a <> " als Datenstrom")

runJson :: Record -> Value
runJson r =
  Object $
    r
      <> KM.fromList
        [ ("_id", String (text "id" r))
        , ("messages_text", String (messagesText (text "messages" r)))
        ]

-- | Meldungen (JSON-Liste) als Text, eine Zeile je Meldung.
messagesText :: Text -> Text
messagesText t = case J.decode (BL.fromStrict (TE.encodeUtf8 t)) :: Maybe [KM.KeyMap Value] of
  Just ms -> T.intercalate "\n" [text "contract_id" m <> ": " <> text "message" m | m <- ms]
  Nothing -> ""

formatPostings :: Call -> [Record] -> IO [Value]
formatPostings call rs = do
  let currencies = Map.keys (Map.fromList [(text "currency" r, ()) | r <- rs])
  decs <- Map.fromList <$> mapM (\c -> (c,) <$> currencyDecimals call c) currencies
  pure (map (posting decs) rs)
  where
    posting decs r =
      let cur = text "currency" r
          amt = maybe "" (formatAmount (Map.findWithDefault 2 cur decs)) (int "amount" r)
          key = T.intercalate "|" (map (encode . (`text` r)) ["company_code", "contract_id", "condition_type", "object_id", "period_from"])
       in Object $
            KM.insert "amount" (String amt) r
              <> KM.fromList
                [ ("_id", String key)
                , ("id", String key)
                , ("_labels", object ["amount" .= (amt <> " " <> cur)])
                ]

encode, decode :: Text -> Text
encode = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8
decode = TE.decodeUtf8Lenient . urlDecode False . TE.encodeUtf8
