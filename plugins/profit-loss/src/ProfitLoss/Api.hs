-- | Schnittstelle des Plugins – ohne eigene Daten, ohne SQL, nichts wird gespeichert.
--
-- | Object.Action | Payload | Ergebnis |
-- |---|---|---|
-- | ProfitLoss.compute | company_code, period_from, period_to, rent_object_id?, building_id?, entity_id?, group_by? (BUILDING, ENTITY) | message, table, download (CSV) |
-- | TrialBalance.compute | company_code, fiscal_year, period_from, period_to, building_id?, entity_id?, account_from?, account_to?, only_open? | message, table, download |
-- | AccountSheet.compute | company_code, fiscal_year, account_number, period_from, period_to, building_id?, entity_id?, contract_id?, partner_id? | message, table, download |
-- | *.formState | FormStateRequest | Vorbelegung (laufendes Jahr bis heute) |
-- | PartnerBalance.compute | company_code, period_from, period_to, partner_kind? (DEBITOR, CREDITOR), partner_id?, account_number?, only_open? | message, table, download |
-- | LedgerReport.contractAccount | company_code, contract_id, period_from?, period_to? | message, table, download, balance |
-- | LedgerReport.partnerAccount | partner_id, company_code?, period_from?, period_to? | message, table, download, balance |
-- | LedgerReport.accountBalance | company_code, account_number, date?, contract_id? | balance, currency, decimals, items |
--
-- LedgerReport hat keine Maske: Vertrag („Vertragskonto …“, „Darlehensstand …“)
-- und Geschäftspartner („Partnerkonto …“) rufen es auf. Alle Geschäftsjahre bis
-- zum Stichtag zählen (das Hauptbuch trägt Salden nicht vor; Anfangsbestände
-- sind gebucht).
--
-- Gelesen wird mit den Rechten des Benutzers: Sachkonten (GLAccountCompany,
-- GLAccount), Einzelposten (JournalEntryItem), zur Zuordnung der Gebäude die
-- Immobilien (Building, RentObject, BusinessEntity) und Vertragsobjekte
-- (ContractObject) – fehlen diese Plugins, bleibt die Zuordnung leer.
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
import Data.List (nub, sortOn)
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
import ProfitLoss.Ledger
import ProfitLoss.Meta (describe, moduleName)
import ProfitLoss.Objects
import ProfitLoss.Report

version :: Text
version = "0.3.0"

profitLossPlugin :: IORef Settings -> Plugin
profitLossPlugin ref =
  (defaultPlugin moduleName version)
    { pluginDescription = "Auswertungen des Hauptbuchs: GuV je Gebäude und Zeitraum, Summen- und Saldenliste, Kontoblatt"
    , pluginCapabilities =
        [ Capability "ProfitLoss" ["compute", "formState"] [] "GuV rechnen: Bildschirm und CSV, nichts wird gespeichert"
        , Capability "TrialBalance" ["compute", "formState"] [] "Summen- und Saldenliste: Bildschirm und CSV"
        , Capability "AccountSheet" ["compute", "formState"] [] "Kontoblatt mit laufendem Saldo: Bildschirm und CSV"
        , Capability "PartnerBalance" ["compute", "formState"] [] "Debitoren und Kreditoren: Salden je Partner und Abstimmkonto mit Summen, Partnerkonto"
        , Capability "LedgerReport" ["contractAccount", "partnerAccount", "accountBalance"] [] "Vertrags- und Partnerkonto, Kontostand – für Vertrag und Geschäftspartner"
        ]
    , pluginDescribe = Just describe
    , pluginConfigure = \_ settings -> writeIORef ref (parseSettings settings)
    , pluginHandle = \call req -> readIORef ref >>= \s -> handler s call req
    }

-- | settings: ledger (Standard 0L), sections: [{title, side: INCOME|EXPENSE, account_types: [...]}],
-- building_field (Kontierungsspalte des Gebäudes, Standard dimension_custom_1).
parseSettings :: KM.KeyMap Value -> Settings
parseSettings o =
  Settings
    { setLedger = nonEmpty "ledger" (setLedger defaultSettings)
    , setSections = case KM.lookup "sections" o of
        Just (Array xs) | ss@(_ : _) <- mapMaybe section (toList xs) -> ss
        _ -> setSections defaultSettings
    , setBuildingField = nonEmpty "building_field" (setBuildingField defaultSettings)
    }
  where
    nonEmpty k def = case KM.lookup k o of
      Just (String l) | not (T.null (T.strip l)) -> T.strip l
      _ -> def
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
  Just (Bool b) -> if b then "true" else "false"
  _ -> ""

int :: Text -> Record -> Maybe Integer
int k r = case KM.lookup (K.fromText k) r of
  Just (Number n) -> toInteger <$> (toBoundedInteger n :: Maybe Int)
  Just (String s) | [(n, "")] <- reads (T.unpack (T.strip s)) -> Just n
  _ -> Nothing

flag :: Text -> Record -> Bool
flag k r = text k r `elem` ["true", "1", "on", "yes"]

day :: Text -> Record -> Maybe Day
day k r = iso8601ParseM (T.unpack (T.take 10 (text k r)))

handler :: Settings -> Call -> Request -> IO Response
handler settings call req = case (reqObject req, reqAction req) of
  (o, "formState") | o `elem` ["ProfitLoss", "TrialBalance", "AccountSheet", "PartnerBalance"] -> do
    today <- utctDay <$> getCurrentTime
    let (y, _, _) = toGregorian today
        values = case KM.lookup "values" p of
          Just (Object v) -> v
          _ -> mempty
        preset k v = [K.fromText k .= object ["value" .= v] | T.null (text k values)]
        -- Vorbelegung als Text (FieldState.value ist eine Zeichenkette)
        year = if o `elem` ["ProfitLoss", "PartnerBalance"] then [] else preset "fiscal_year" (T.pack (show y))
    pure . response $
      object
        [ "fields" .= object (year <> preset "period_from" (iso8601Show (fromGregorian y 1 1)) <> preset "period_to" (iso8601Show today))
        , "message" .= formMessage o
        ]
  ("ProfitLoss", "compute") -> do
    cc <- required "company_code" "Buchungskreis"
    (from, to) <- period
    response <$> profitLoss settings call cc p from to
  ("TrialBalance", "compute") -> do
    cc <- required "company_code" "Buchungskreis"
    fy <- year
    (from, to) <- period
    response <$> balanceList settings call cc fy p from to
  ("AccountSheet", "compute") -> do
    cc <- required "company_code" "Buchungskreis"
    acc <- required "account_number" "Sachkonto"
    fy <- year
    (from, to) <- period
    response <$> sheet settings call cc fy acc p from to
  ("PartnerBalance", "compute") -> do
    cc <- required "company_code" "Buchungskreis"
    (from, to) <- period
    response <$> partnerList settings call cc p from to
  ("LedgerReport", "contractAccount") -> do
    cc <- required "company_code" "Buchungskreis"
    c <- required "contract_id" "Vertrag"
    to <- optDate "period_to" "Zeitraum bis"
    response <$> contractAccount settings call cc c (optDay "period_from") to
  ("LedgerReport", "partnerAccount") -> do
    bp <- required "partner_id" "Geschäftspartner"
    to <- optDate "period_to" "Zeitraum bis"
    response <$> partnerAccount settings call (text "company_code" p) bp (optDay "period_from") to
  ("LedgerReport", "accountBalance") -> do
    cc <- required "company_code" "Buchungskreis"
    acc <- required "account_number" "Sachkonto"
    to <- optDate "date" "Stichtag"
    response <$> accountBalance settings call cc acc (text "contract_id" p) to
  (o, a) -> pluginError Unimplemented (o <> "." <> a)
  where
    optDay k = day k p
    optDate k label
      | T.null (text k p) = utctDay <$> getCurrentTime
      | otherwise = date k label
    p = params (reqPayload req)
    required k label = case text k p of
      "" -> pluginError InvalidArgument (label <> " ist Pflicht")
      v -> pure v
    date k label = do
      v <- required k label
      maybe (pluginError InvalidArgument (label <> ": Datum JJJJ-MM-TT erwartet, nicht " <> v)) pure (iso8601ParseM (T.unpack (T.take 10 v)))
    period = do
      from <- date "period_from" "Zeitraum von"
      to <- date "period_to" "Zeitraum bis"
      if from > to then pluginError InvalidArgument "Zeitraum: „von“ liegt nach „bis“" else pure (from, to)
    year = maybe (pluginError InvalidArgument "Geschäftsjahr ist Pflicht (Zahl, z. B. 2026)") pure (int "fiscal_year" p)

formMessage :: Text -> Text
formMessage = \case
  "TrialBalance" -> "Salden aller Konten im Geschäftsjahr: Vortrag vor „von“, Soll und Haben im Zeitraum. Ausgeglichen = Saldo 0."
  "PartnerBalance" -> "Debitoren und Kreditoren je Abstimmkonto mit Summen. Mit einem Geschäftspartner: sein Partnerkonto mit laufendem Saldo und Stand je Vertrag."
  "AccountSheet" -> "Alle Buchungen eines Kontos mit laufendem Saldo – z. B. Darlehenskonto, Kautionen, Mieter- oder Lieferantenkonto."
  _ -> "Gewinn und Verlust aus den gebuchten Einzelposten (Buchungsdatum im Zeitraum), wahlweise für ein Gebäude oder mit einer Spalte je Gebäude."

-- | Einzelposten mit Gebäude und Wirtschaftseinheit.
data Line = Line
  { lnRecord :: Record
  , lnAccount :: Text
  , lnDate :: Day
  , lnAmount :: Integer
  , lnYear :: Maybe Integer
  , lnBuilding :: Text
  , lnEntity :: Text
  }

-- | Einzelposten des Buchungskreises im Ledger der Einstellungen (Filter zusätzlich zur Abfrage).
readLines :: Settings -> Call -> Objects -> Text -> [(Text, Value)] -> IO ([Line], Text)
readLines settings call objs cc extra = do
  raw <- readAll call "JournalEntryItem" ([("company_code_id", String cc), ("ledger", String (setLedger settings))] <> extra)
  let ls =
        [ Line r (text "account_number" r) d a (int "fiscal_year" r) b (entityOfPosting objs b contract d)
        | r <- raw
        , T.null (text "ledger" r) || text "ledger" r == setLedger settings
        , and [text k r == v | (k, String v) <- extra]
        , Just d <- [day "posting_date" r]
        , Just a <- [int "amount_local_curr" r]
        , let contract = text "rent_contract_id" r
              b = buildingOf objs (text (setBuildingField settings) r) (text "rent_object_id" r) contract d
        ]
      currency = fromMaybe "EUR" (lookup True [(True, c) | r <- raw, let c = text "local_currency" r, not (T.null c)])
  pure (ls, currency)

-- | Filter Gebäude und Wirtschaftseinheit der Maske (leer = alle).
objectFilter :: Record -> Line -> Bool
objectFilter p l =
  (T.null building || lnBuilding l == building) && (T.null entity || lnEntity l == entity)
  where
    building = text "building_id" p
    entity = text "entity_id" p

-- | Gebäude, Mietobjekte, Wirtschaftseinheiten und Vertragsobjekte (fehlt ein Plugin: leer).
loadObjects :: Call -> Text -> IO Objects
loadObjects call cc = do
  buildings <- tolerant "Building"
  rentObjects <- tolerant "RentObject"
  entities <- tolerant "BusinessEntity"
  contracts <- tolerant "ContractObject"
  pure
    Objects
      { obBuildings = Map.fromList [(text "building_id" b, (text "designation" b, text "entity_id" b)) | b <- buildings]
      , obRentObjects = Map.fromList [(text "object_id" r, text "building_id" r) | r <- rentObjects, not (T.null (text "building_id" r))]
      , obEntities = Map.fromList [(text "entity_id" e, text "designation" e) | e <- entities]
      , obContracts =
          Map.fromListWith
            (<>)
            [ (text "contract_id" c, [ContractObject (text "object_type" c) (text "object_id" c) (flag "is_main" c) (day "valid_from" c) (day "valid_to" c)])
            | c <- contracts
            ]
      }
  where
    tolerant obj = do
      r <- try (readAll call obj [("company_code", String cc)])
      pure $ case r of
        Right xs -> xs
        Left (_ :: PluginError) -> []

-- | Sachkonten des Buchungskreises mit Bezeichnung und Kontotyp.
loadAccounts :: Call -> Text -> IO [Account]
loadAccounts call cc = do
  company <- readAll call "GLAccountCompany" [("company_code_id", String cc)]
  if null company then pluginError InvalidArgument ("Buchungskreis " <> cc <> ": keine Sachkonten (oder keine Berechtigung)") else pure ()
  masters <-
    concat <$> mapM (\ch -> readAll call "GLAccount" [("chart_of_accounts_id", String ch)]) (nub (filter (not . T.null) (map (text "chart_of_accounts_id") company)))
  let master = Map.fromList [(text "account_number" m, m) | m <- masters]
  pure
    [ Account n (if T.null (text "account_name" c) then maybe "" (text "name") m else text "account_name" c) (maybe "" (text "account_type") m)
    | c <- company
    , let n = text "account_number" c
          m = Map.lookup n master
    ]

-- | Angaben der Maske für Kopf und Meldung: Gebäude, Wirtschaftseinheit, Mietobjekt.
selection :: Objects -> Record -> [(Text, Text)]
selection objs p =
  [("Gebäude", buildingName objs b) | let b = text "building_id" p, not (T.null b)]
    <> [("Wirtschaftseinheit", entityName objs e) | let e = text "entity_id" p, not (T.null e)]
    <> [("Mietobjekt", o) | let o = text "rent_object_id" p, not (T.null o)]

profitLoss :: Settings -> Call -> Text -> Record -> Day -> Day -> IO Value
profitLoss settings call cc p from to = do
  accounts <- loadAccounts call cc
  let obj = text "rent_object_id" p
      groupBy = text "group_by" p
  objs <- loadObjects call cc
  (ls, currency) <- readLines settings call objs cc [("rent_object_id", String obj) | not (T.null obj)]
  dec <- currencyDecimals call currency
  let used = filter (objectFilter p) ls
      key l = case groupBy of
        "ENTITY" -> lnEntity l
        _ -> lnBuilding l
      item l = Item (lnAccount l) (lnDate l) (lnAmount l) (key l)
      items = map item used
      rep = computeReport settings accounts from to items
      groups =
        [ (g, computeReport settings accounts from to [i | i <- items, itGroup i == g])
        | g <- sortOn (\g -> (T.null g, label g)) (nub (map itGroup items))
        ]
      label g = if groupBy == "ENTITY" then entityName objs g else buildingName objs g
      grouped = groupBy `elem` ["BUILDING", "ENTITY"]
      active = [(g, r) | (g, r) <- groups, not (null (rpSections r))]
      (columns, rows)
        | grouped = (tableColumnsBy [label g | (g, _) <- active], tableRowsBy dec rep active)
        | otherwise = (tableColumns, tableRows dec rep)
      period = formatDay from <> "–" <> formatDay to
      chosen = selection objs p
      header = [("GuV-Rechnung", ""), ("Buchungskreis", cc)] <> chosen <> [("Zeitraum", period), ("Währung", currency)]
      file = "GuV_" <> cc <> T.concat ["_" <> safe v | (_, v) <- chosen] <> "_" <> T.pack (iso8601Show from) <> "_" <> T.pack (iso8601Show to) <> ".csv"
      message =
        "GuV Buchungskreis " <> cc <> T.concat [", " <> k <> " " <> v | (k, v) <- chosen] <> ", " <> period <> ": "
          <> T.pack (show (rpItems rep)) <> " Einzelposten, Ergebnis "
          <> formatAmountDe dec (rpResult rep) <> " " <> currency
          <> (if rpResult rep >= 0 then " (Gewinn)" else " (Verlust)")
          <> unassigned
      unassigned =
        case lookup "" groups of
          Just r | rpItems r > 0, T.null (text "building_id" p), T.null (text "entity_id" p) ->
            "; davon ohne Gebäude: " <> T.pack (show (rpItems r)) <> " Einzelposten, " <> formatAmountDe dec (rpResult r) <> " " <> currency
          _ -> ""
  pure (result message columns rows header file)

balanceList :: Settings -> Call -> Text -> Integer -> Record -> Day -> Day -> IO Value
balanceList settings call cc fy p from to = do
  accounts <- loadAccounts call cc
  objs <- loadObjects call cc
  (ls, currency) <- readLines settings call objs cc []
  dec <- currencyDecimals call currency
  let lo = text "account_from" p
      hi = text "account_to" p
      inRange n = (T.null lo || n >= lo) && (T.null hi || n <= hi)
      postings = [posting objs l | l <- ls, lnYear l == Just fy, objectFilter p l, inRange (lnAccount l)]
      rows = trialBalance accounts from to postings
      onlyOpen = flag "only_open" p
      open = length (filter ((/= 0) . brClosing) rows)
      period = formatDay from <> "–" <> formatDay to
      chosen = selection objs p
      header = [("Summen- und Saldenliste", ""), ("Buchungskreis", cc), ("Geschäftsjahr", T.pack (show fy))] <> chosen <> [("Zeitraum", period), ("Währung", currency)]
      file = "Saldenliste_" <> cc <> "_" <> T.pack (show fy) <> T.concat ["_" <> safe v | (_, v) <- chosen] <> "_" <> T.pack (iso8601Show to) <> ".csv"
      message =
        "Saldenliste " <> cc <> " " <> T.pack (show fy) <> T.concat [", " <> k <> " " <> v | (k, v) <- chosen] <> ", " <> period <> ": "
          <> T.pack (show (length rows)) <> " Konten, davon " <> T.pack (show open) <> " nicht ausgeglichen"
  pure (result message balanceColumns (balanceRows dec onlyOpen rows) header file)

sheet :: Settings -> Call -> Text -> Integer -> Text -> Record -> Day -> Day -> IO Value
sheet settings call cc fy acc p from to = do
  accounts <- loadAccounts call cc
  objs <- loadObjects call cc
  (ls, currency) <- readLines settings call objs cc [("account_number", String acc)]
  dec <- currencyDecimals call currency
  partners <- partnerNames call
  let contract = text "contract_id" p
      partner = text "partner_id" p
      keep l =
        lnYear l == Just fy && objectFilter p l
          && (T.null contract || text "rent_contract_id" (lnRecord l) == contract)
          && (T.null partner || partnerOf (lnRecord l) == partner)
      postings = [(posting objs l) {poPartner = named partners (poPartner (posting objs l))} | l <- ls, keep l]
      sh = accountSheet from to postings
      name = maybe "" accName (lookup acc [(accNumber a, a) | a <- accounts])
      period = formatDay from <> "–" <> formatDay to
      chosen = selection objs p <> [("Vertrag", contract) | not (T.null contract)] <> [("Partner", named partners partner) | not (T.null partner)]
      header = [("Kontoblatt", ""), ("Buchungskreis", cc), ("Konto", acc <> " " <> name), ("Geschäftsjahr", T.pack (show fy))] <> chosen <> [("Zeitraum", period), ("Währung", currency)]
      file = "Kontoblatt_" <> cc <> "_" <> safe acc <> "_" <> T.pack (show fy) <> T.concat ["_" <> safe v | (k, v) <- chosen, k /= "Partner"] <> "_" <> T.pack (iso8601Show to) <> ".csv"
      message =
        "Konto " <> acc <> " " <> name <> T.concat [", " <> k <> " " <> v | (k, v) <- chosen] <> ": Saldo am " <> formatDay to <> " "
          <> formatAmountDe dec (shClosing sh) <> " " <> currency <> " – " <> balanceText (shClosing sh)
          <> " (" <> T.pack (show (length (shPostings sh))) <> " Buchungen im Zeitraum)"
  pure (result message sheetColumns (sheetRows dec from to sh) header file)

-- | Partnerposten (Debitoren und Kreditoren) eines Ledgers bis zum Stichtag.
partnerLine :: Settings -> Record -> Bool
partnerLine settings r =
  text "account_kind" r `elem` ["D", "K"] && (T.null (text "ledger" r) || text "ledger" r == setLedger settings)

-- | Vertragskonto: Partnerposten der Belege zum Vertrag – Belege mit einem Posten
-- zum Vertrag (Sollstellung, Rechnung) und Bankbelege der Umsätze mit diesem
-- Vertrag. Posten eines anderen Vertrags im selben Beleg zählen nicht.
contractAccount :: Settings -> Call -> Text -> Text -> Maybe Day -> Day -> IO Value
contractAccount settings call cc contract mfrom to = do
  objs <- loadObjects call cc
  linked <- readAll call "JournalEntryItem" [("company_code_id", String cc), ("rent_contract_id", String contract)]
  bank <- tolerantAll call "BankTransaction" [("company_code", String cc), ("contract_id", String contract)]
  let known = nub [(text "fiscal_year" r, text "document_number" r) | r <- linked]
      extra = nub [d | b <- bank, let d = text "document_number" b, not (T.null d), d `notElem` map snd known]
  others <- concat <$> mapM (\d -> readAll call "JournalEntryItem" [("company_code_id", String cc), ("document_number", String d)]) extra
  docs <- concat <$> mapM (\(fy, d) -> readAll call "JournalEntryItem" [("company_code_id", String cc), ("fiscal_year", String fy), ("document_number", String d)]) known
  partners <- partnerNames call
  let items =
        dedupe
          [ r
          | r <- docs <> others
          , partnerLine settings r
          , text "rent_contract_id" r `elem` ["", contract]
          ]
  report settings call objs partners ("Vertragskonto " <> contract) [("Vertrag", contract)] ("Vertragskonto_" <> cc <> "_" <> safe contract) items mfrom to False

-- | Partnerkonto: alle Debitoren- und Kreditorenposten des Geschäftspartners, je Vertrag mit Stand.
partnerAccount :: Settings -> Call -> Text -> Text -> Maybe Day -> Day -> IO Value
partnerAccount settings call cc bp mfrom to = do
  let ccFilter = [("company_code_id", String cc) | not (T.null cc)]
  customer <- readAll call "JournalEntryItem" (("sd_customer_id", String bp) : ccFilter)
  supplier <- readAll call "JournalEntryItem" (("supplier_id", String bp) : ccFilter)
  partners <- partnerNames call
  let items = dedupe [r | r <- customer <> supplier, partnerLine settings r]
  objs <- if T.null cc then pure noObjects else loadObjects call cc
  report settings call objs partners ("Partnerkonto " <> named partners bp) ([("Buchungskreis", cc) | not (T.null cc)] <> [("Partner", named partners bp)]) ("Partnerkonto_" <> safe bp) items mfrom to True

-- | Debitoren und Kreditoren: ohne Partner je Abstimmkonto und Partner mit Summen,
-- mit Partner dessen Partnerkonto im Buchungskreis.
partnerList :: Settings -> Call -> Text -> Record -> Day -> Day -> IO Value
partnerList settings call cc p from to
  | not (T.null (text "partner_id" p)) = partnerAccount settings call cc (text "partner_id" p) (Just from) to
  | otherwise = do
      accounts <- loadAccounts call cc
      raw <- readAll call "JournalEntryItem" ([("company_code_id", String cc)] <> [("account_number", String acc) | not (T.null acc)])
      partners <- partnerNames call
      let kinds = case text "partner_kind" p of
            "DEBITOR" -> ["D"]
            "CREDITOR" -> ["K"]
            _ -> ["D", "K"]
          postings =
            [ (k, Posting (text "account_number" r) d (text "document_number" r) "" "" (partnerOf r) "" a)
            | r <- raw
            , partnerLine settings r
            , let k = text "account_kind" r
            , k `elem` kinds
            , T.null acc || text "account_number" r == acc
            , Just d <- [day "posting_date" r]
            , Just a <- [int "amount_local_curr" r]
            ]
          currency = fromMaybe "EUR" (lookup True [(True, c) | r <- raw, let c = text "local_currency" r, not (T.null c)])
          rows = partnerBalances from to postings
          names = Map.fromList [(accNumber x, accName x) | x <- accounts]
          open = length (filter ((/= 0) . prClosing) rows)
          period = formatDay from <> "–" <> formatDay to
          kindText = case kinds of
            ["D"] -> "Debitoren"
            ["K"] -> "Kreditoren"
            _ -> "Debitoren und Kreditoren"
          header = [(kindText, ""), ("Buchungskreis", cc)] <> [("Konto", acc) | not (T.null acc)] <> [("Zeitraum", period), ("Währung", currency)]
          file = T.replace " " "_" kindText <> "_" <> cc <> "_" <> T.pack (iso8601Show to) <> ".csv"
          closing k = sum [prClosing r | r <- rows, prKind r == k]
      dec <- currencyDecimals call currency
      let message =
            kindText <> " " <> cc <> ", " <> period <> ": " <> T.pack (show (length rows)) <> " Partnerkonten, davon " <> T.pack (show open) <> " nicht ausgeglichen"
              <> T.concat [", Saldo " <> n <> " " <> formatAmountDe dec (closing k) <> " " <> currency | (k, n) <- [("D", "Debitoren"), ("K", "Kreditoren")], k `elem` kinds]
      pure (result message partnerListColumns (partnerListRows dec (flag "only_open" p) (\a -> Map.findWithDefault "" a names) (\x -> Map.findWithDefault "" x partners) rows) header file)
  where
    acc = text "account_number" p

-- | Stand eines Sachkontos am Stichtag (alle Geschäftsjahre), wahlweise nur Posten eines Vertrags.
accountBalance :: Settings -> Call -> Text -> Text -> Text -> Day -> IO Value
accountBalance settings call cc acc contract to = do
  raw <- readAll call "JournalEntryItem" ([("company_code_id", String cc), ("account_number", String acc)] <> [("rent_contract_id", String contract) | not (T.null contract)])
  let items =
        [ a
        | r <- raw
        , T.null (text "ledger" r) || text "ledger" r == setLedger settings
        , T.null contract || text "rent_contract_id" r == contract
        , Just d <- [day "posting_date" r]
        , d <= to
        , Just a <- [int "amount_local_curr" r]
        ]
      currency = fromMaybe "EUR" (lookup True [(True, c) | r <- raw, let c = text "local_currency" r, not (T.null c)])
  dec <- currencyDecimals call currency
  pure (object ["balance" .= sum items, "items" .= length items, "currency" .= currency, "decimals" .= dec, "date" .= iso8601Show to])

-- | Gemeinsame Ausgabe von Vertrags- und Partnerkonto.
report :: Settings -> Call -> Objects -> Map.Map Text Text -> Text -> [(Text, Text)] -> Text -> [Record] -> Maybe Day -> Day -> Bool -> IO Value
report _ call objs partners title chosen file items mfrom to perContract = do
  let currency = fromMaybe "EUR" (lookup True [(True, c) | r <- items, let c = text "local_currency" r, not (T.null c)])
  dec <- currencyDecimals call currency
  let postings =
        [ Posting
            { poAccount = text "account_number" r
            , poDate = d
            , poDocument = text "document_number" r
            , poText = text "item_text" r
            , poContract = text "rent_contract_id" r
            , poPartner = named partners (partnerOf r)
            , poBuilding = let b = buildingOf objs "" (text "rent_object_id" r) (text "rent_contract_id" r) d in if T.null b then "" else buildingName objs b
            , poAmount = a
            }
        | r <- items
        , Just d <- [day "posting_date" r]
        , Just a <- [int "amount_local_curr" r]
        ]
      from = fromMaybe (minimum (to : map poDate postings)) mfrom
      sh = accountSheet from to postings
      totals = contractTotals to postings
      rows
        | perContract = partnerRows dec from to totals sh
        | otherwise = sheetRows dec from to sh
      period = formatDay from <> "–" <> formatDay to
      header = [(title, "")] <> chosen <> [("Zeitraum", period), ("Währung", currency)]
      open = [ctContract t | t <- totals, ctClosing t /= 0]
      message =
        title <> ": Saldo am " <> formatDay to <> " " <> formatAmountDe dec (shClosing sh) <> " " <> currency <> " – " <> balanceText (shClosing sh)
          <> " (" <> T.pack (show (length (shPostings sh))) <> " Posten im Zeitraum)"
          <> (if perContract && length totals > 1 then "; nicht ausgeglichen: " <> (if null open then "keiner" else T.intercalate ", " [if T.null c then "ohne Vertrag" else c | c <- open]) else "")
  pure $ case result message sheetColumns rows header (file <> "_" <> T.pack (iso8601Show to) <> ".csv") of
    Object o -> Object (KM.insert "balance" (Number (fromInteger (shClosing sh))) o)
    v -> v

-- | Posten nur einmal (gleicher Schlüssel aus zwei Abfragen; der Datenstrom list hat keine id).
dedupe :: [Record] -> [Record]
dedupe = Map.elems . Map.fromList . map (\r -> (map (`text` r) ["company_code_id", "fiscal_year", "document_number", "ledger", "line_item_number"], r))

tolerantAll :: Call -> Text -> [(Text, Value)] -> IO [Record]
tolerantAll call obj ps = do
  r <- try (readAll call obj ps)
  pure $ case r of
    Right xs -> xs
    Left (_ :: PluginError) -> []

posting :: Objects -> Line -> Posting
posting objs l =
  Posting
    { poAccount = lnAccount l
    , poDate = lnDate l
    , poDocument = text "document_number" r
    , poText = text "item_text" r
    , poContract = text "rent_contract_id" r
    , poPartner = partnerOf r
    , poBuilding = if T.null (lnBuilding l) then "" else buildingName objs (lnBuilding l)
    , poAmount = lnAmount l
    }
  where
    r = lnRecord l

partnerOf :: Record -> Text
partnerOf r = let c = text "sd_customer_id" r in if T.null c then text "supplier_id" r else c

-- | Geschäftspartner → Name (fehlt das Plugin: leer, dann bleibt die Nummer).
partnerNames :: Call -> IO (Map.Map Text Text)
partnerNames call = do
  r <- try (readAll call "BusinessPartner" [])
  pure $ case r of
    Right xs -> Map.fromList [(text "id" b, T.strip (text "name1" b <> " " <> text "name2" b)) | b <- xs]
    Left (_ :: PluginError) -> mempty

named :: Map.Map Text Text -> Text -> Text
named names p
  | T.null p = ""
  | Just n <- Map.lookup p names, not (T.null n) = p <> " " <> n
  | otherwise = p

result :: Text -> [Text] -> [[Text]] -> [(Text, Text)] -> Text -> Value
result message columns rows header file =
  object
    [ "message" .= message
    , "table" .= object ["columns" .= columns, "rows" .= rows]
    , "download" .= object ["content" .= content, "content_type" .= ("text/csv;charset=utf-8" :: Text), "file_name" .= file]
    ]
  where
    content = TE.decodeUtf8 (convertToBase Base64 (TE.encodeUtf8 ("\xFEFF" <> csv header columns rows)) :: ByteString)

safe :: Text -> Text
safe = T.map (\c -> if c `elem` ("/\\:*?\"<>| ()" :: String) then '-' else c)

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
