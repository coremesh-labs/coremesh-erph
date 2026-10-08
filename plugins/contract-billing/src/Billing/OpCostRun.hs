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
--     Positionen der Kostenart, anteilig nach Abrechnungszeitraum.
module Billing.OpCostRun
  ( computeOpCostRun
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
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
      values t = Set.fromList (map ruSourceValue (collects t))
  -- LEDGER
  ledgerSources <-
    concat
      <$> mapM
        ( \acc -> do
            -- Regel: Konto mit Kontenplan-Präfix (SKR25-6300), Einzelposten ohne
            items <- readAll sys "JournalEntryItem" [("company_code_id", String cc), ("account_number", String (shortAccount acc))]
            pure
              [ Source "LEDGER" acc ref amt tgt formula ""
              | it <- items
              , Just d <- [day "posting_date" it]
              , d >= from && d <= to
              , T.null (text "ledger" it) || text "ledger" it == ledger
              , Just amt <- [int "amount_local_curr" it]
              , Just tgt <- [target (textMaybe "rent_contract_id" it) "RentObject" (text "rent_object_id" it)]
              , let docNo = text "document_number" it
                    ref = "JE:" <> docNo <> "/" <> text "line_item_number" it
                    formula = "Beleg " <> docNo <> " Pos. " <> text "line_item_number" it <> " vom " <> formatDay d <> textSuffix (text "item_text" it)
              ]
        )
        (Set.toList (values "LEDGER"))
  -- INVOICE
  invoiceSources <-
    if Set.null (values "INVOICE")
      then pure []
      else do
        heads <- Map.fromList . map (\r -> (text "invoice_id" r, r)) . filter ((== "POSTED") . text "status") <$> readAll sys "SupplierInvoice" byCC
        items <- filter (\r -> text "cost_category" r `Set.member` values "INVOICE" && activeRow r) <$> readAll sys "SupplierInvoiceItem" byCC
        pure
          [ Source "INVOICE" (text "cost_category" it) ref amt tgt formula ""
          | it <- items
          , Just h <- [Map.lookup (text "invoice_id" it) heads]
          , let (objType, objId) =
                  if T.null (text "object_id" it) then (text "object_type" h, text "object_id" h) else (text "object_type" it, text "object_id" it)
          , Just tgt <- [target (textMaybe "contract_id" it) objType objId]
          , Just total <- [int "amount" it]
          , Just (amt, part) <- [share period total (day "service_from" it) (day "service_to" it) (day "posting_date" h)]
          , let ref = "INV:" <> text "invoice_id" it <> "/" <> text "line_no" it
                formula =
                  "Rechnung " <> text "invoice_id" it <> " Pos. " <> text "line_no" it <> " " <> text "supplier_id" h
                    <> textSuffix (text "supplier_reference" h) <> ": " <> formatAmount 2 total <> part <> textSuffix (text "item_text" it)
          ]
  -- CONTRACT_SETTLEMENT
  settlementSources <-
    if Set.null (values "CONTRACT_SETTLEMENT")
      then pure []
      else do
        heads <-
          Map.fromList . map (\r -> ((text "contract_id" r, T.take 10 (text "period_from" r)), r)) . filter ((== "POSTED") . text "status")
            <$> readAll sys "ContractSettlement" byCC
        items <- filter (\r -> text "cost_category" r `Set.member` values "CONTRACT_SETTLEMENT" && activeRow r) <$> readAll sys "ContractSettlementItem" byCC
        let contractTarget cid =
              let os = [co | co <- cobjects, oContract co == cid, overlap period (oFrom co, oTo co) /= Nothing]
                  mine = Set.toList (Set.fromList [oId co | co <- os, oType co == "RentObject", oId co `Set.member` objectIds])
               in case mine of
                    [o] -> Just (ToObject o)
                    _ : _ -> Just ToPool
                    [] -> case [() | co <- os, Just ToPool == target Nothing (oType co) (oId co)] of
                      _ : _ -> Just ToPool
                      [] -> Nothing
        pure
          [ Source "CONTRACT_SETTLEMENT" (text "cost_category" it) ref amt tgt formula ""
          | it <- items
          , let cid = text "contract_id" it
                pf = T.take 10 (text "period_from" it)
          , Just h <- [Map.lookup (cid, pf) heads]
          , Just tgt <- [if T.null (text "object_id" it) then contractTarget cid else target Nothing (text "object_type" it) (text "object_id" it)]
          , Just total <- [int "amount" it]
          , Just sf <- [day "period_from" h]
          , Just st <- [day "period_to" h]
          , Just (amt, part) <- [share period total (Just sf) (Just st) Nothing]
          , let ref = "CS:" <> cid <> "/" <> pf <> "/" <> text "line_no" it
                keyText = case (textMaybe "total_cost" it, textMaybe "key_share" it, textMaybe "key_total" it) of
                  (Just tc, Just ks, Just kt) ->
                    " (Gesamtkosten " <> maybe tc (formatAmount 2) (int "total_cost" it) <> " × " <> ks <> " / " <> kt <> textSuffix (text "allocation_key" it) <> ")"
                  _ -> ""
                formula =
                  "Abrechnung " <> cid <> " " <> formatDay sf <> "–" <> formatDay st <> " Pos. " <> text "line_no" it <> ": "
                    <> formatAmount 2 total <> keyText <> part
          ]
  -- Vorauszahlungen der Mieter im Zeitraum (anteilig)
  postings <- concatMap (maybe [] pure . posted) <$> readAll sys "ContractPosting" byCC
  let advances =
        Map.fromListWith (+)
          [ (pContract x, prorate (pAmount x) (pFrom x, pTo x) iv)
          | x <- postings
          , pContract x `Set.member` tenantSet
          , pType x `elem` advanceTypes
          , Just iv <- [overlap period (pFrom x, pTo x)]
          ]
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
            , opSources = ledgerSources <> invoiceSources <> settlementSources
            , opAdvances = advances
            , opCategories = Map.fromList [(text "code" c, text "name" c) | c <- objects "categories" p]
            }
      checks = orChecks res
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
                 ]
             | t <- orTenants res
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

-- | Anteil eines Betrags im Zeitraum: nach Leistungszeitraum (Tage), sonst
-- ganz, wenn das Buchungsdatum im Zeitraum liegt. Text: Zeitanteil.
share :: (Day, Day) -> Integer -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe (Integer, Text)
share period total sf st posting = case (sf, st) of
  (Just a, Just b) | a <= b -> do
    iv <- overlap period (a, b)
    let part
          | iv == (a, b) = " (Leistung " <> formatDay a <> "–" <> formatDay b <> ")"
          | otherwise = " × " <> tshow (days iv) <> " / " <> tshow (days (a, b)) <> " Tage (Leistung " <> formatDay a <> "–" <> formatDay b <> ")"
    pure (prorate total (a, b) iv, part)
  _ -> do
    d <- posting
    if d >= fst period && d <= snd period then Just (total, "") else Nothing

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
      }

allocKey :: Record -> AllocKey
allocKey r = AllocKey (text "code" r) (text "name" r) (text "basis" r) (textMaybe "measurement_type" r)

person :: Record -> Maybe PersonSlice
person r = do
  from <- day "valid_from" r
  n <- int "persons" r
  pure (PersonSlice (text "contract_id" r) n from (fromMaybe openEnd (day "valid_to" r)))

levelText :: Level -> Text
levelText = \case
  Ok -> "ok"
  Deviation -> "Abweichung"
  Info -> "Hinweis"
  Failure -> "Fehler"

tshow :: Show a => a -> Text
tshow = T.pack . show
