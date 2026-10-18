-- | Nebenkostenabrechnung rechnen (ContractBilling.computeOpCost) – lädt die
-- Daten der Einheit als Datenstrom und rechnet mit "Billing.OpCost".
--
-- Das Betriebskostenmodul (opcost) schickt Regelwerk, Regeln und
-- Verteilerschlüssel, speichert Journal, Ergebnis je Mieter und Prüfungen,
-- gibt frei und bucht. Die Berechtigung prüft opcost (SettlementRun.compute).
--
-- Quellen (je Sammelregel):
--
--   * LEDGER: Einzelposten des Sachkontos im Ledger des Regelwerks mit
--     Buchungsdatum im Zeitraum; kontiert auf einen Mietvertrag der Einheit →
--     Mieter, auf ein Mietobjekt der Einheit → Objekt, auf ein fremdes
--     Mietobjekt → nicht diese Einheit, sonst → Topf;
--   * INVOICE: gebuchte Eingangsrechnungen, Positionen der Kostenart;
--     Leistungszeitraum anteilig, ohne Leistungszeitraum das Buchungsdatum;
--   * CONTRACT_SETTLEMENT: gebuchte Vertragsabrechnungen (WEG, Versorger),
--     Positionen der Kostenart, anteilig nach Abrechnungszeitraum;
--   * INPUT (seit 0.19.0): Eingaben am Lauf (Konto = Wert der Regel) – mit
--     Vertrag und Wohnung auf den Mieter je Wohnung, mit Vertrag auf den
--     Mieter, nur mit Wohnung auf das Objekt, sonst in den Topf.
--
-- LEDGER: mehrere Sachkonten mit Komma (z. B. Personal- und Materialkosten).
-- ADVANCE (Vorauszahlungen einer Ebene): BOOKED = Sollstellungen der
-- Konditionsarten (anteilig), INPUT = Eingaben am Lauf je Vertrag.
module Billing.OpCostRun
  ( computeOpCostRun
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Control.Monad (join)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, addGregorianYearsClip)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import Billing.Cache
import Billing.Data
import Billing.Loan (parseRate)
import Billing.OpCost
import Billing.Plan (formatAmount, formatDay)
import Billing.Schedule (prorate)
import Billing.Settlement (Check (..), Level (..))
import CoreMesh.Plugin

-- | Verträge, deren Nutzung zählt (wie bei der Sollstellung).
tenantStatuses :: [Text]
tenantStatuses = ["ACTIVE", "TERMINATED"]

computeOpCostRun :: Cache -> Call -> Record -> IO Value
computeOpCostRun cache call p = do
  cc <- req "company_code" p "Buchungskreis"
  defRec <- maybe (pluginError InvalidArgument "Regelwerk fehlt") pure (objectField "definition" p)
  from <- dateReq "period_from"
  to <- dateReq "period_to"
  unitType <- req "unit_type" defRec "Abrechnungseinheit (Art)"
  unit <- req "unit_id" defRec "Abrechnungseinheit"
  let scale = fromMaybe 100000 (int "scale" defRec)
      period = (from, to)
      list' k = [T.strip x | x <- T.splitOn "," (text k defRec), not (T.null (T.strip x))]
      tenantTypes = list' "tenant_contract_types"
      advanceTypes = list' "advance_types"
      ledger = fromMaybe "0L" (textMaybe "ledger" defRec)
      rules = mapMaybe rule (objects "rules" p)
      keys = Map.fromList [(akCode k, k) | k <- map allocKey (objects "keys" p)]
  if scale <= 0 then pluginError InvalidArgument "Rechengenauigkeit: Faktor > 0 erwartet" else pure ()
  if from > to then pluginError InvalidArgument "Zeitraum: von liegt nach bis" else pure ()
  sys <- systemCall call (callTenantId call)
  let get = slice cache sys cc
      byCC = [("company_code", String cc)]
  -- Einheit: Gebäude und Mietobjekte
  buildings <-
    if unitType == "Building"
      then pure (Set.singleton unit)
      else Set.fromList . map (text "building_id") . filter ((== unit) . text "entity_id") <$> readAll sys "Building" byCC
  rentObjects <- filter (\r -> text "building_id" r `Set.member` buildings || (unitType == "BusinessEntity" && text "entity_id" r == unit)) <$> readAll sys "RentObject" byCC
  let objectIds = Set.fromList (map (text "object_id") rentObjects)
  -- Mieter: Verträge der Mietervertragsarten, Nutzung der Mietobjekte im Zeitraum
  contracts <- concatMap (maybe [] pure . contract) <$> get Contracts
  let tenantContracts = Map.fromList [(cId c, c) | c <- contracts, cType c `elem` tenantTypes, cStatus c `elem` tenantStatuses]
  cobjects <- concatMap (maybe [] pure . contractObject) <$> get Objects
  let occupancy =
        [ Occupancy (cId c) (cPartner c) (oId co) a b
        | co <- cobjects
        , oType co == "RentObject"
        , oId co `Set.member` objectIds
        , Just c <- [Map.lookup (oContract co) tenantContracts]
        , Just (a, b) <- [overlap period (cFrom c, cTo c) >>= overlap (oFrom co, oTo co)]
        ]
      tenantSet = Set.fromList (map ocContract occupancy)
  persons <-
    if any (\k -> akBasis k == "PERSONS") (Map.elems keys)
      then
        mapMaybe person . filter ((`Set.member` tenantSet) . text "contract_id")
          <$> readAll sys "ContractPersons" (byCC <> [("includeHistory", Bool True)])
      else pure []
  measurements <-
    if any (\k -> akBasis k == "MEASUREMENT") (Map.elems keys)
      then filter ((`Set.member` objectIds) . mObject) . concatMap (maybe [] pure . measurement) <$> get Measurements
      else pure []
  -- Kontierung → Ziel; Nothing = gehört nicht zu dieser Einheit
  let target mContract objType objId
        | Just c <- mContract, c `Set.member` tenantSet = Just (ToTenant c)
        | T.null objId = Just ToPool
        | objType == "RentObject" || T.null objType = if objId `Set.member` objectIds then Just (ToObject objId) else Nothing
        | objType == "Building" = if objId `Set.member` buildings then Just ToPool else Nothing
        | objType == "BusinessEntity" = if unitType == "BusinessEntity" && objId == unit then Just ToPool else Nothing
        | otherwise = Nothing
      collects t = [r | r <- rules, ruKind r == "COLLECT", ruSourceType r == t]
      inPeriod d = d >= from && d <= to
      -- Zuordnung nach Datum: Buchungs- oder Belegdatum im Zeitraum, dann ganz
      dated r posting document = case ruAssignment r of
        "DOCUMENT_DATE" -> (,) <$> document <*> pure (" (Belegdatum)" :: Text)
        _ -> (,) <$> posting <*> pure (" (Buchungsdatum)" :: Text)
      whole total (d, label) = if inPeriod d then Just (total, label <> " " <> formatDay d) else Nothing
  -- LEDGER: Einzelposten haben keinen Leistungszeitraum – Buchungs- oder Belegdatum
  docDates <-
    if any ((== "DOCUMENT_DATE") . ruAssignment) (collects "LEDGER")
      then
        Map.fromList . map (\h -> ((text "document_number" h, text "fiscal_year" h), day "document_date" h))
          <$> readAll sys "JournalEntry" [("company_code_id", String cc)]
      else pure mempty
  -- Objektkontierung im Hauptbuch (Beschaffung → ObjectPosting): welches Feld der
  -- Buchungszeile welche Objektart trägt; Reihenfolge Mietobjekt, Gebäude, Wirtschaftseinheit.
  objPostingRecs <- if null (collects "LEDGER") then pure [] else readAllOrEmpty sys "ObjectPosting" byCC
  let objFields =
        let configured = Map.fromList [(text "object_type" r, text "ledger_field" r) | r <- objPostingRecs, not (T.null (text "ledger_field" r))]
            fields = Map.union configured (Map.fromList [("RentObject", "rent_object_id"), ("Building", "dimension_custom_1"), ("BusinessEntity", "dimension_custom_2")])
         in [(t, f) | t <- ["RentObject", "Building", "BusinessEntity"], Just f <- [Map.lookup t fields]]
      -- erstes gefülltes Objektfeld der Zeile; ohne Objekt: ganze Abrechnungseinheit
      itemObject it = case [(t, v) | (t, f) <- objFields, let v = text f it, not (T.null v)] of
        o : _ -> o
        [] -> ("RentObject", "")
  ledgerSources <-
    concat
      <$> mapM
        ( \r -> do
            -- Regel und Einzelposten: reine Kontonummer (ledger 0.15.0); der Filter des Hauptbuchs kürzt Altbestand mit Präfix
            items <- concat <$> mapM (\acc -> readAll sys "JournalEntryItem" [("company_code_id", String cc), ("account_number", String acc)]) (codes (ruSourceValue r))
            pure
              [ Source "LEDGER" (text "account_number" it) ref amt tgt formula "" (ruNo r) Nothing
              | it <- items
              , let docNo = text "document_number" it
              , Just dl <- [dated r (day "posting_date" it) (join (Map.lookup (docNo, text "fiscal_year" it) docDates))]
              , inPeriod (fst dl)
              , T.null (text "ledger" it) || text "ledger" it == ledger
              , Just amt <- [int "amount_local_curr" it]
              , let (objType, objId) = itemObject it
              , Just tgt <- [target (textMaybe "rent_contract_id" it) objType objId]
              , let ref = "JE:" <> docNo <> "/" <> text "line_item_number" it
                    formula = "Beleg " <> docNo <> " Pos. " <> text "line_item_number" it <> snd dl <> " " <> formatDay (fst dl) <> textSuffix (text "item_text" it)
              ]
        )
        (collects "LEDGER")
  -- INVOICE: Leistungszeitraum anteilig (ohne: Buchungsdatum mit Hinweis), Buchungs- oder Rechnungsdatum
  invoiceSources <-
    if null (collects "INVOICE")
      then pure []
      else do
        heads <- Map.fromList . map (\r -> (text "invoice_id" r, r)) . filter ((== "POSTED") . text "status") <$> readAll sys "SupplierInvoice" byCC
        items <- filter activeRow <$> readAll sys "SupplierInvoiceItem" byCC
        let assign r h it total = case ruAssignment r of
              "SERVICE_PERIOD" -> case (day "service_from" it, day "service_to" it) of
                (Just a, Just b) | a <= b -> (\(x, t) -> (x, t, Nothing)) <$> periodShare period total (a, b)
                _ ->
                  (\(x, t) -> (x, t, Just ("Rechnung " <> text "invoice_id" it <> " Pos. " <> text "line_no" it <> " ohne Leistungszeitraum – nach Buchungsdatum zugeordnet")))
                    <$> (dated r {ruAssignment = "POSTING_DATE"} (day "posting_date" h) Nothing >>= whole total)
              _ -> (\(x, t) -> (x, t, Nothing)) <$> (dated r (day "posting_date" h) (day "invoice_date" h) >>= whole total)
        pure
          [ Source "INVOICE" (ruSourceValue r) ref amt tgt formula "" (ruNo r) warning
          | r <- collects "INVOICE"
          , it <- items
          , T.null (text "dependent_type" it) -- nachgelagerte Positionen stecken im Kostenbetrag der Basis
          , text "cost_category" it == ruSourceValue r
          , Just h <- [Map.lookup (text "invoice_id" it) heads]
          , let (objType, objId) =
                  if T.null (text "object_id" it) then (text "object_type" h, text "object_id" h) else (text "object_type" it, text "object_id" it)
          , Just tgt <- [target (textMaybe "contract_id" it) objType objId]
          -- seit 0.13.0: Kostenbetrag (netto − Anteil + verteilte Zuschläge und nicht abziehbare Steuer), sonst Betrag
          , Just total <- [maybe (int "amount" it) Just (int "cost_amount" it)]
          , Just (amt, part, warning) <- [assign r h it total]
          , let ref = "INV:" <> text "invoice_id" it <> "/" <> text "line_no" it
                formula =
                  "Rechnung " <> text "invoice_id" it <> " Pos. " <> text "line_no" it <> " " <> text "supplier_id" h
                    <> textSuffix (text "supplier_reference" h) <> ": " <> formatAmount 2 total <> part <> textSuffix (text "item_text" it)
          ]
  -- CONTRACT_SETTLEMENT: Abrechnungszeitraum anteilig, Buchungs- oder Abrechnungsdatum; Vollständigkeit je Vertrag
  (settlementSources, coverage) <-
    if null (collects "CONTRACT_SETTLEMENT")
      then pure ([], [])
      else do
        heads <- Map.fromList . map (\r -> ((text "contract_id" r, T.take 10 (text "period_from" r)), r)) <$> readAll sys "ContractSettlement" byCC
        items <- filter activeRow <$> readAll sys "ContractSettlementItem" byCC
        let contractTarget cid =
              let os = [co | co <- cobjects, oContract co == cid, overlap period (oFrom co, oTo co) /= Nothing]
                  mine = Set.toList (Set.fromList [oId co | co <- os, oType co == "RentObject", oId co `Set.member` objectIds])
               in case mine of
                    [o] -> Just (ToObject o)
                    _ : _ -> Just ToPool
                    [] -> case [() | co <- os, Just ToPool == target Nothing (oType co) (oId co)] of
                      _ : _ -> Just ToPool
                      [] -> Nothing
            itemTarget it = if T.null (text "object_id" it) then contractTarget (text "contract_id" it) else target Nothing (text "object_type" it) (text "object_id" it)
            headOf it = Map.lookup (text "contract_id" it, T.take 10 (text "period_from" it)) heads
            isPosted h = text "status" h == "POSTED"
            hPeriod h = (,) <$> day "period_from" h <*> day "period_to" h
            assign r h total = case ruAssignment r of
              "SERVICE_PERIOD" -> hPeriod h >>= periodShare period total
              _ -> dated r (day "posting_date" h) (day "settlement_date" h) >>= whole total
            prev = (addGregorianYearsClip (-1) from, addGregorianYearsClip (-1) to)
            contractOf' = Map.fromList [(cId c, c) | c <- contracts]
            srcs =
              [ Source "CONTRACT_SETTLEMENT" (ruSourceValue r) ref amt tgt formula "" (ruNo r) Nothing
              | r <- collects "CONTRACT_SETTLEMENT"
              , it <- items
              , text "cost_category" it == ruSourceValue r
              , Just h <- [headOf it]
              , isPosted h
              , Just tgt <- [itemTarget it]
              , Just total <- [int "amount" it]
              , Just (sf, st) <- [hPeriod h]
              , Just (amt, part) <- [assign r h total]
              , let cid = text "contract_id" it
                    ref = "CS:" <> cid <> "/" <> T.take 10 (text "period_from" it) <> "/" <> text "line_no" it
                    keyText = case (textMaybe "total_cost" it, textMaybe "key_share" it, textMaybe "key_total" it) of
                      (Just tc, Just ks, Just kt) ->
                        " (Gesamtkosten " <> maybe tc (formatAmount 2) (int "total_cost" it) <> " × " <> ks <> " / " <> kt <> textSuffix (text "allocation_key" it) <> ")"
                      _ -> ""
                    formula =
                      "Abrechnung " <> cid <> " " <> formatDay sf <> "–" <> formatDay st <> " Pos. " <> text "line_no" it <> ": "
                        <> formatAmount 2 total <> keyText <> part
              ]
            covs =
              [ Coverage (ruNo r) cid required covered previous unposted (ruAssignment r == "SERVICE_PERIOD")
              | r <- collects "CONTRACT_SETTLEMENT"
              , let mine = [(it, h) | it <- items, text "cost_category" it == ruSourceValue r, Just h <- [headOf it], itemTarget it /= Nothing]
              , cid <- Set.toList (Set.fromList [text "contract_id" it | (it, _) <- mine])
              , let hs = Map.elems (Map.fromList [(text "period_from" h, h) | (it, h) <- mine, text "contract_id" it == cid])
                    covered =
                      [ iv
                      | h <- hs
                      , isPosted h
                      , Just iv <- [ if ruAssignment r == "SERVICE_PERIOD"
                                       then hPeriod h >>= overlap period
                                       else (\(d, _) -> (d, d)) <$> (dated r (day "posting_date" h) (day "settlement_date" h) >>= \dl -> if inPeriod (fst dl) then Just dl else Nothing)
                                   ]
                      ]
                    previous = or [isPosted h && maybe False (isJust . overlap prev) (hPeriod h) | h <- hs]
                    unposted =
                      [ formatDay a <> "–" <> formatDay b <> " (" <> text "status" h <> ")"
                      | h <- hs
                      , text "status" h `elem` ["OPEN", "DRAFT"]
                      , Just (a, b) <- [hPeriod h]
                      , isJust (overlap period (a, b))
                      ]
              , Just required <- [maybe (Just period) (\c -> overlap period (cFrom c, cTo c)) (Map.lookup cid contractOf')]
              , not (null covered) || previous || not (null unposted)
              ]
        pure (srcs, covs)
  -- Eingaben am Lauf (Heizkosten, CO2-Gutschrift, WEG-Gesamtkosten, Vorauszahlungen)
  let inputs = [(text "account" i, text "contract_id" i, text "rent_object_id" i, a) | i <- objects "inputs" p, Just a <- [int "amount" i]]
      inputSources =
        [ Source "INPUT" acc ("IN:" <> acc <> ":" <> c <> ":" <> o) a tgt ("Eingabe " <> acc <> textSuffix c <> textSuffix o <> ": " <> formatAmount 2 a) "" (ruNo r) Nothing
        | r <- collects "INPUT"
        , (acc, c, o, a) <- inputs
        , acc == ruSourceValue r
        , Just tgt <- [inputTarget c o]
        ]
      inputTarget c o
        | not (T.null c) = if c `Set.member` tenantSet then Just (if T.null o then ToTenant c else ToTenantObject c o) else Nothing
        | not (T.null o) = if o `Set.member` objectIds then Just (ToObject o) else Nothing
        | otherwise = Just ToPool
      inputChecks =
        [ Check ("Eingabe " <> acc) Nothing (Just a) Failure ("Vertrag " <> c <> " ist kein Mieter der Einheit im Zeitraum" <> textSuffix o)
        | (acc, c, o, a) <- inputs
        , not (T.null c)
        , not (c `Set.member` tenantSet)
        ]
  -- Vorauszahlungen der Mieter im Zeitraum (anteilig)
  postings <- concatMap (maybe [] pure . posted) <$> readAll sys "ContractPosting" byCC
  let advancesOf types =
        Map.fromListWith (+)
          [ (pContract x, prorate (pAmount x) (pFrom x, pTo x) iv)
          | x <- postings
          , pContract x `Set.member` tenantSet
          , pType x `elem` types
          , Just iv <- [overlap period (pFrom x, pTo x)]
          ]
      advances = advancesOf advanceTypes
      ruleValues =
        Map.fromList
          [ (ruNo r, vals)
          | r <- rules
          , ruKind r == "ADVANCE"
          , let vals
                  | ruSourceType r == "INPUT" = Map.fromListWith (+) [(c, a) | (acc, c, _, a) <- inputs, acc == ruSourceValue r, not (T.null c)]
                  | otherwise = advancesOf (codes (ruSourceValue r))
          ]
      accountDefs = Map.fromList [(text "code" a, AccountDef (text "code" a) (text "name" a) (fromMaybe "CONTRACT" (textMaybe "binding" a))) | a <- objects "accounts" p]
      def = Def unit scale (fromMaybe "TENANTS_OR_OWNER" (textMaybe "rounding_rule" defRec)) from to
      res =
        computeOpCost
          OpInput
            { opDef = def
            , opRules = rules
            , opKeys = keys
            , opObjects = Set.toList objectIds
            , opMeasurements = measurements
            , opOccupancy = occupancy
            , opPersons = persons
            , opSources = ledgerSources <> invoiceSources <> settlementSources <> inputSources
            , opAdvances = advances
            , opCoverage = coverage
            , opCategories = Map.fromList [(text "code" c, text "name" c) | c <- objects "categories" p]
            , opAccounts = accountDefs
            , opRuleValues = ruleValues
            }
      checks = orChecks res <> inputChecks
      count l = length [() | c <- checks, chLevel c == l]
      checkSummary = tshow (count Ok) <> " ok, " <> tshow (count Deviation) <> " Abweichungen, " <> tshow (count Failure) <> " Fehler"
      summaryText =
        "Kosten " <> formatAmount 2 (orSources res) <> " · Mieter " <> formatAmount 2 (orTenantTotal res) <> " · Eigentümer/Leerstand "
          <> formatAmount 2 (orOwnerTotal res) <> " · Vorauszahlungen " <> formatAmount 2 (sum (map trAdvances (orTenants res)))
          <> " · Prüfung: " <> checkSummary
      checkLines =
        T.intercalate "\n"
          [ levelText (chLevel c) <> " – " <> chName c <> ": " <> chText c <> maybe "" (\a -> " (" <> formatAmount 2 a <> ")") (chActual c)
          | c <- checks
          ]
  pure $
    object
      [ "summary" .= summaryText
      , "failures" .= count Failure
      , "check_result" .= (checkSummary <> "\n" <> checkLines)
      , "message" .= ("Abrechnung gerechnet: " <> summaryText)
      , "journal"
          .= [ object
                 [ "step" .= jStep l, "rule" .= jRule l, "debit" .= accText (jDebit l), "credit" .= accText (jCredit l)
                 , "amount" .= jAmount l, "formula" .= jFormula l, "source" .= jSource l, "cost_category" .= jCategory l
                 ]
             | l <- orJournal res
             ]
      , "tenants"
          .= [ object
                 [ "contract_id" .= trContract t, "partner_id" .= trPartner t, "rent_object_id" .= trObject t, "usage_from" .= iso8601Show (trFrom t)
                 , "usage_to" .= iso8601Show (trTo t), "costs" .= trCosts t, "advances" .= trAdvances t
                 , "lines" .= T.intercalate "; " [k <> " " <> formatAmount 2 v | (k, v) <- trLines t]
                 , "post_lines" .= [object ["rule" .= plRule l, "cost" .= plCost l, "advance" .= plAdvance l] | l <- trPost t]
                 ]
             | t <- orTenants res
             ]
      , "results"
          .= [ object
                 [ "contract_id" .= rlContract l, "rent_object_id" .= rlObject l, "account" .= rlAccount l, "level" .= rlLevel l
                 , "amount" .= rlAmount l, "formula" .= rlFormula l
                 ]
             | t <- orTenants res
             , l <- trResults t
             ]
      , "occupancy"
          .= [ object ["contract_id" .= ocContract o, "partner_id" .= ocPartner o, "rent_object_id" .= ocObject o, "from" .= iso8601Show (ocFrom o), "to" .= iso8601Show (ocTo o)]
             | o <- occupancy
             ]
      , "table"
          .= object
            [ "columns" .= (["Vertrag", "Mieter", "Nutzung", "Kosten", "Vorauszahlungen", "Nachzahlung/Guthaben"] :: [Text])
            , "rows"
                .= [ [ trContract t, trPartner t, formatDay (trFrom t) <> "–" <> formatDay (trTo t), formatAmount 2 (trCosts t)
                     , formatAmount 2 (trAdvances t), formatAmount 2 (trCosts t - trAdvances t)
                     ]
                   | t <- orTenants res
                   ]
            ]
      ]
  where
    req k r label = maybe (pluginError InvalidArgument (label <> " ist Pflicht")) pure (textMaybe k r)
    dateReq k = do
      t <- req k p k
      maybe (pluginError InvalidArgument (k <> ": Datum erwartet, nicht " <> t)) pure (iso8601ParseM (T.unpack (T.take 10 t)) :: Maybe Day)
    activeRow r = case KM.lookup "is_active" r of
      Nothing -> True
      Just Null -> True
      _ -> bool "is_active" r
    textSuffix t = if T.null (T.strip t) then "" else " " <> T.strip t

-- | Anteil eines Betrags im Zeitraum nach Leistungs- bzw. Abrechnungszeitraum
-- (Tage). Text: Zeitanteil.
periodShare :: (Day, Day) -> Integer -> (Day, Day) -> Maybe (Integer, Text)
periodShare period total (a, b) = do
  iv <- overlap period (a, b)
  let part
        | iv == (a, b) = " (Leistung " <> formatDay a <> "–" <> formatDay b <> ")"
        | otherwise = " × " <> tshow (days iv) <> " / " <> tshow (days (a, b)) <> " Tage (Leistung " <> formatDay a <> "–" <> formatDay b <> ")"
  pure (prorate total (a, b) iv, part)

objectField :: Text -> Record -> Maybe Record
objectField k r = case KM.lookup (K.fromText k) r of
  Just (Object o) -> Just o
  _ -> Nothing

objects :: Text -> Record -> [Record]
objects k r = case KM.lookup (K.fromText k) r of
  Just (Array xs) -> [o | Object o <- foldr (:) [] xs]
  _ -> []

rule :: Record -> Maybe Rule
rule r = do
  no <- int "rule_no" r
  pure
    Rule
      { ruNo = no
      , ruStep = fromMaybe 0 (int "step" r)
      , ruDescription = text "description" r
      , ruKind = text "kind" r
      , ruCategory = text "cost_category" r
      , ruSourceType = text "source_type" r
      , ruSourceValue = text "source_value" r
      , ruPool = text "pool" r
      , ruToPool = text "to_pool" r
      , ruShare = fromMaybe 0 (textMaybe "share_pct" r >>= parseRate)
      , ruKey = text "allocation_key" r
      , ruTimeSplit = not (bool "vacancy_to_tenants" r)
      , ruAssignment = fromMaybe (if text "source_type" r == "LEDGER" then "POSTING_DATE" else "SERVICE_PERIOD") (textMaybe "assignment" r)
      , ruInputs = codes (text "source_value" r)
      }

allocKey :: Record -> AllocKey
allocKey r = AllocKey (text "code" r) (text "name" r) (text "basis" r) (textMaybe "measurement_type" r)

person :: Record -> Maybe PersonSlice
person r = do
  from <- day "valid_from" r
  n <- int "persons" r
  pure (PersonSlice (text "contract_id" r) n from (fromMaybe openEnd (day "valid_to" r)))

-- | Codes mit Komma ("A,B" → ["A", "B"]).
codes :: Text -> [Text]
codes t = [T.strip x | x <- T.splitOn "," t, not (T.null (T.strip x))]

levelText :: Level -> Text
levelText = \case
  Ok -> "ok"
  Deviation -> "Abweichung"
  Info -> "Hinweis"
  Failure -> "Fehler"

tshow :: Show a => a -> Text
tshow = T.pack . show
