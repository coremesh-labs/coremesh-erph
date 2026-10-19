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
  , depreciationPlan
  , SettlementRequest (..)
  , settlementCheck
  , settlementPost
  , currencyDecimals
  ) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)

import Billing.Cache
import Billing.Data
import Billing.Depreciation
import Billing.Loan
import Billing.Plan
import Billing.Pricing
import Billing.Schedule (Due (..), roundDiv)
import Billing.Settlement
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
  typeRecs <- get Types
  kindRecs <- get ConditionKinds
  let types = map contractType typeRecs
  allContracts <- only cId . concatMap (maybe [] pure . contract) <$> get Contracts
  contracts <- visibleContracts call allContracts
  let ids = Set.fromList (map cId contracts)
      mine :: (a -> Text) -> [a] -> [a]
      mine f = filter ((`Set.member` ids) . f)
  -- Rechnungsvorlagen (Abrechnungsweg Eingangsrechnung, z. B. Dienstleister, Rahmenvertrag)
  -- erzeugen keine Sollstellung.
  condRecs <- filter (not . isTemplate typeRecs kindRecs) <$> get Conditions
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
  depRecs <- get Depreciations
  let parsedDeps = [(text "contract_id" r, depTerms r) | r <- depRecs]
      deps = mine dpContract [d | (_, Right d) <- parsedDeps]
      badDeps = [(c, "Abschreibungskondition nicht lesbar: " <> e) | (c, Left e) <- parsedDeps, c `Set.member` ids]
  accounts <- map account <$> get Accounts
  status <- fieldStatus <$> get FieldStatuses
  postings <- postedOf sys rrCompany
  -- Buchungskreisdaten der Partner (Abstimmkonto, Buchungssperre) – wie die Vermerke immer frisch
  partnerAccounts <- map partnerAccount <$> readAll sys "PartnerCompanyCode" [("company_code", String rrCompany)]
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  -- Steuerung des Buchungskreises im Hauptbuch: „rückwirkend buchen ab“
  ledgerCfg <- readAllOrEmpty sys "LedgerCompanyConfig" [("company_code_id", String rrCompany)]
  let backdateFrom = listToMaybe ledgerCfg >>= day "backdate_from"
      companyRounding = listToMaybe ledgerCfg >>= textMaybe "rounding_rule"
  -- Positionen (seit 0.13.0): Konditionsarten, nachgelagerte Arten, Steuerkennzeichen, Rundungsregeln
  condTypes <- map condType <$> get ConditionTypes
  depTypes <- map dependentType <$> get DependentTypes
  let needTaxes = any (isJust . kDependent) conditions
  taxes <- if needTaxes then concatMap (maybe [] pure . taxCode) <$> get TaxCodes else pure []
  roundings <- concatMap (maybe [] pure . rounding) <$> get RoundingRules
  decimals <- mapM (\cur -> (cur,) <$> currencyDecimals call cur) (nub (map cCurrency contracts))
  rentBuildings <- Map.fromList . concatMap (maybe [] pure . rentBuilding) <$> get RentObjects
  fields <- objectFields cache
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
          , inDepreciations = deps
          , inPartnerAccounts = Map.fromList [((paPartner a, paRole a), a) | a <- partnerAccounts]
          , inPosted = postings
          , inCorrectionDate = min today rrTo
          , inBackdateFrom = backdateFrom
          , inConditionTypes = Map.fromList [(ktCode t, t) | t <- condTypes]
          , inDependentTypes = Map.fromList [(dCode t, t) | t <- depTypes]
          , inTaxCodes = Map.fromList [(tcCode t, t) | t <- taxes]
          , inRoundings = Map.fromList [(rCode r, r) | r <- roundings]
          , inCompanyRounding = companyRounding
          , inDecimals = Map.fromList decimals
          , inRentBuildings = rentBuildings
          , inObjectFields = fields
          }
      (docs, planMsgs) = plan rrTo input
      msgs0 = badConditions <> badLoans <> badDeps <> planMsgs
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
        , "position" .= kPos k
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
loanPlan :: Cache -> Call -> Text -> Text -> Maybe Day -> IO Value
loanPlan cache call cc contractId until = do
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
      let horizon = maybe (fromGregorian 2199 12 31) id until
          rows = [r | r <- schedule mineL [p | p <- payments, lpContract p == contractId] (cTo k) horizon, maybe True (rDue r <=) until]
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

-- | Abschreibungsplan eines Vertrags (Aktion „Abschreibungsplan“): je Jahr bis
-- zum Buchwert 0 bzw. Vertragsende, als Tabelle.
depreciationPlan :: Cache -> Call -> Text -> Text -> IO Value
depreciationPlan cache call cc contractId = do
  sys <- systemCall call (callTenantId call)
  let get = slice cache sys cc
  terms <- concatMap (either (const []) pure . depTerms) <$> get Depreciations
  contracts <- concatMap (maybe [] pure . contract) <$> get Contracts
  let mine = [d | d <- terms, dpContract d == contractId]
      c = [x | x <- contracts, cId x == contractId]
  case (mine, c) of
    ([], _) -> pluginError NotFound ("Vertrag " <> contractId <> " hat keine Abschreibungskondition")
    (_, []) -> pluginError NotFound ("Vertrag " <> contractId <> " gibt es im Buchungskreis " <> cc <> " nicht")
    (_, k : _) -> do
      dec <- currencyDecimals call (cCurrency k)
      let rows = depSchedule mine (cTo k) (fromGregorian 2199 12 31)
          amt = formatAmount dec
          total = sum (map drAmount rows)
          lastYear = case reverse rows of
            r : _ -> formatDay (drDue r)
            [] -> "–"
      pure $
        object
          [ "message"
              .= ( "Abschreibungsplan " <> contractId <> ": " <> tshow (length rows) <> " Jahre bis " <> lastYear <> ", Abschreibung gesamt "
                    <> amt total <> " " <> cCurrency k
                 )
          , "table"
              .= object
                [ "columns" .= (["Jahr bis", "Monate", "Raten", "Buchwert Anfang", "Zugang", "Abschreibung", "Buchwert Ende"] :: [Text])
                , "rows"
                    .= [ [formatDay (drDue r), tshow (drMonths r), tshow (length (depInstallments r)), amt (drBookBefore r - drAddition r), amt (drAddition r), amt (drAmount r), amt (drBookAfter r)]
                       | r <- rows
                       ]
                ]
          ]

-- * Vertragsabrechnung ----------------------------------------------------------

data SettlementRequest = SettlementRequest
  { srCompany :: Text
  , srContract :: Text
  , srFrom :: Day
  , srAdvanceTypes :: [Text] -- ^ Konditionsarten „Vorauszahlung (wird abgerechnet)“
  , srTolerance :: Text -- ^ Dezimaltext, z. B. 0.05
  }

data Loaded = Loaded
  { ldContract :: Contract
  , ldType :: ContractType
  , ldHead :: SetHead
  , ldItems :: [SetItem]
  , ldAdvances :: [Posted] -- ^ gebuchte Vorauszahlungen des Zeitraums
  , ldDecimals :: Int
  , ldChecks :: [Check]
  }

setHead :: Record -> Maybe SetHead
setHead r = do
  from <- day "period_from" r
  to <- day "period_to" r
  d <- day "settlement_date" r
  pure
    SetHead
      { shFrom = from, shTo = to, shDate = d, shPosting = maybe d id (day "posting_date" r), shReference = text "reference" r
      , shStatedAdvances = int "stated_advances" r, shStatedResult = int "stated_result" r
      , shReserveOpening = int "reserve_opening" r, shReserveWithdrawal = maybe 0 id (int "reserve_withdrawal" r)
      , shReserveClosing = int "reserve_closing" r, shReserveAccount = textMaybe "reserve_account" r
      , shWithdrawalAccount = textMaybe "withdrawal_account" r, shStatus = text "status" r
      }

setItem :: Record -> Maybe (Day, SetItem)
setItem r = do
  from <- day "period_from" r
  amount <- int "amount" r
  let dec k = textMaybe k r >>= parseRate
  pure
    ( from
    , SetItem
        { siLine = maybe 0 fromInteger (int "line_no" r), siGroup = text "settlement_group" r, siCategory = text "cost_category" r
        , siCostType = text "cost_type" r, siTotal = int "total_cost" r, siKey = text "allocation_key" r
        , siKeyTotal = dec "key_total", siKeyShare = dec "key_share", siAmount = amount, siAccount = text "account_number" r
        , siObjectType = text "object_type" r, siObject = text "object_id" r
        }
    )

-- | Abrechnung, Vorjahr, Vorauszahlungen und Rücklagensaldo laden und prüfen.
loadSettlement :: Cache -> Call -> SettlementRequest -> IO Loaded
loadSettlement cache call SettlementRequest {..} = do
  sys <- systemCall call (callTenantId call)
  let get = slice cache sys srCompany
      flt = [("company_code", String srCompany), ("contract_id", String srContract)]
  contracts <- concatMap (maybe [] pure . contract) <$> get Contracts
  typeRecs <- get Types
  kindRecs <- get ConditionKinds
  let types = map contractType typeRecs
  c <- case [x | x <- contracts, cId x == srContract] of
    x : _ -> pure x
    [] -> pluginError NotFound ("Vertrag " <> srContract <> " gibt es im Buchungskreis " <> srCompany <> " nicht")
  ct <- case [t | t <- types, ctCode t == cType c] of
    t : _ -> pure t
    [] -> pluginError NotFound ("Vertragsart " <> cType c)
  heads <- concatMap (maybe [] pure . setHead) <$> readAll sys "ContractSettlement" flt
  itemRecs <- readAll sys "ContractSettlementItem" (flt <> [("is_active", Bool True)])
  let items = [(f, i) | Just (f, i) <- map setItem itemRecs]
      itemsOf f = [i | (g, i) <- items, g == f]
  h <- case [x | x <- heads, shFrom x == srFrom] of
    x : _ -> pure x
    [] -> pluginError NotFound ("Abrechnung " <> srContract <> " ab " <> formatDay srFrom <> " gibt es nicht")
  let prev = case reverse (sortOnFrom [x | x <- heads, shFrom x < srFrom]) of
        p : _ -> Just (p, itemsOf (shFrom p))
        [] -> Nothing
  postings <- postedOf sys srCompany
  let advances =
        [ p | p <- postings, pContract p == srContract, pType p `elem` srAdvanceTypes, pFrom p >= shFrom h, pFrom p <= shTo h ]
  dec <- currencyDecimals call (cCurrency c)
  let tol = maybe 0 (\r -> roundRational (r * 10 ^ dec)) (parseRate srTolerance)
      mine = itemsOf srFrom
      reserveAcc = case shReserveAccount h of
        Just a -> Just a
        Nothing -> case [siAccount i | i <- mine, siCostType i == "RESERVE"] of
          a : _ -> Just a
          [] -> Nothing
      reserveObj = case [siObject i | i <- mine, siCostType i == "RESERVE", siObjectType i == "RentObject"] of
        o : _ -> Just o
        [] -> Nothing
  ledger <- case reserveAcc of
    Nothing -> pure Nothing
    Just acc -> reserveBalance call srCompany acc reserveObj (shTo h)
  pure (Loaded c ct h mine advances dec (checks tol h mine (sum (map pAmount advances)) prev ledger))
  where
    sortOnFrom = map snd . Map.toList . Map.fromList . map (\x -> (shFrom x, x))
    roundRational r = roundDiv (numerator r) (denominator r)

-- | Saldo des Rücklagenkontos zum Tag (Hauswährung), optional je Mietobjekt.
reserveBalance :: Call -> Text -> Text -> Maybe Text -> Day -> IO (Maybe Integer)
reserveBalance call cc acc obj to = do
  r <- try (dispatch call "AccountBalance" "list" (object (["company_code" .= cc, "date_to" .= iso8601Show to] <> maybe [] (\o -> ["rent_object_id" .= o]) obj)))
  pure $ case r of
    Left (_ :: PluginError) -> Nothing
    Right (Object o) | Just (Array xs) <- KM.lookup "items" o ->
      Just (sum [n | Object it <- foldr (:) [] xs, Just (String a) <- [KM.lookup "account_number" it], sameAcc a, Just (Number n') <- [KM.lookup "balance_minor" it], let n = floor n'])
    _ -> Just 0
  where
    sameAcc a = a == acc || T.isSuffixOf ("-" <> acc) a || T.isSuffixOf ("-" <> a) acc

checksJson :: Int -> [Check] -> Value
checksJson dec cs =
  object
    [ "columns" .= (["Prüfung", "Soll", "Ist", "Abweichung", "Ergebnis", "Erläuterung"] :: [Text])
    , "rows"
        .= [ [ chName c, maybe "" (formatAmount dec) (chExpected c), maybe "" (formatAmount dec) (chActual c)
             , case (chExpected c, chActual c) of
                 (Just e, Just a) -> formatAmount dec (a - e)
                 _ -> ""
             , levelText (chLevel c), chText c
             ]
           | c <- cs
           ]
    ]

levelText :: Level -> Text
levelText = \case
  Ok -> "ok"
  Deviation -> "Abweichung"
  Info -> "Hinweis"
  Failure -> "Fehler"

summary :: [Check] -> Text
summary cs =
  let n l = length [() | c <- cs, chLevel c == l]
   in tshow (n Ok) <> " ok, " <> tshow (n Deviation) <> " Abweichungen, " <> tshow (n Failure) <> " Fehler"

-- | „Prüfen“: Prüfungen als Tabelle.
settlementCheck :: Cache -> Call -> SettlementRequest -> IO Value
settlementCheck cache call req = do
  ld <- loadSettlement cache call req
  let cs = ldChecks ld
  pure $ object ["message" .= ("Prüfung: " <> summary cs), "check_result" .= summary cs, "table" .= checksJson (ldDecimals ld) cs]

-- | „Buchen …“: prüfen, Vorerfassung anlegen, prüfen im Hauptbuch, ggf. buchen,
-- Buchung an der Abrechnung vermerken – in einer Transaktion.
settlementPost :: Cache -> Call -> SettlementRequest -> IO Value
settlementPost cache call req@SettlementRequest {..} = do
  ld <- loadSettlement cache call req
  let h = ldHead ld
      c = ldContract ld
      ct = ldType ld
      cs = ldChecks ld
  case [chText x | x <- cs, chLevel x == Failure] of
    e : _ -> pluginError InvalidArgument ("Abrechnung nicht buchbar: " <> e)
    [] -> pure ()
  if shStatus h /= "OPEN" then pluginError InvalidArgument "Abrechnung ist nicht erfasst (bereits vorerfasst oder gebucht)" else pure ()
  if ctDirection ct /= "PAYABLE" then pluginError InvalidArgument "Vertragsabrechnung: nur für Verträge, bei denen wir zahlen" else pure ()
  sys <- systemCall call (callTenantId call)
  partnerRecs <- readAll sys "PartnerCompanyCode" [("company_code", String srCompany), ("bp_id", String (cPartner c)), ("role_code", String (ctRole ct))]
  recon <- case [a | r <- partnerRecs, let pa = partnerAccount r, not (paPostingBlock pa), Just a <- [paAccount pa]] of
    a : _ -> pure a
    [] -> pluginError InvalidArgument ("Partnerkonto fehlt oder Buchungssperre: " <> cPartner c <> " in der Rolle " <> ctRole ct)
  accounts <- map account <$> slice cache sys srCompany Accounts
  status <- fieldStatus <$> slice cache sys srCompany FieldStatuses
  let label = "Abrechnung " <> formatDay (shFrom h) <> "–" <> formatDay (shTo h)
      advancesByAccount = [(pAccount p, pAmount p) | p <- ldAdvances ld]
      diff = itemTotal (ldItems ld) - sum (map snd advancesByAccount)
      docType
        | diff < 0 = maybe "KG" id (ctCreditDocumentType ct)
        | otherwise = maybe "KR" id (ctDocumentType ct)
      accMap = Map.fromList [(aNumber a, a) | a <- accounts]
      allowed acc fs = case Map.lookup (shortAcc acc) accMap of
        Nothing -> fs
        Just a ->
          let grp = if T.null (aGroup a) then "STD" else aGroup a
           in [f | f@(n, _) <- fs, Map.lookup (grp, n) status `elem` [Just "OPTIONAL", Just "REQUIRED"]]
  plan' <- either (pluginError InvalidArgument) pure (settlementLines label h (ldItems ld) advancesByAccount recon)
  let dec = ldDecimals ld
  r <- try $ inTx call database $ \tx -> do
    draft <-
      dispatch tx "JournalDraft" "create" $
        object
          [ "data"
              .= object
                [ "company_code_id" .= srCompany, "document_type" .= docType, "posting_date" .= iso8601Show (shPosting h)
                , "document_date" .= iso8601Show (shDate h), "currency" .= cCurrency c
                , "header_text" .= (label <> " " <> cId c), "reference" .= (if T.null (shReference h) then cId c else shReference h)
                ]
          ]
    draftId <- strField "id" draft
    let fieldsFor PlanLine {..} =
          [("rent_contract_id", cId c)]
            <> [("rent_object_id", o) | Just ("RentObject", o) <- [plObject]]
            <> [("supplier_id", cPartner c) | plPartner]
    mapM_
      ( \l@PlanLine {..} ->
          dispatch tx "JournalDraftItem" "create" $
            object
              [ "data"
                  .= object
                    ( [ "draft_id" .= draftId, "account_number" .= plAccount, "shkzg" .= plSide, "amount" .= formatAmount dec plAmount
                      , "item_text" .= plText
                      ]
                        <> [K.fromText k .= v | (k, v) <- allowed plAccount (fieldsFor l)]
                    )
              ]
      )
      plan'
    _ <- dispatch tx "JournalDraft" "simulate" (object ["id" .= draftId])
    (docId, docNo) <-
      if ctAutoPost ct
        then do
          res <- dispatch tx "JournalDraft" "post" (object ["id" .= draftId])
          (,) <$> (Just <$> strField "id" res) <*> (Just <$> strField "document_number" res)
        else pure (Nothing, Nothing)
    _ <-
      dispatch tx "ContractPostingService" "recordSettlement" $
        object
          [ "company_code" .= srCompany, "contract_id" .= srContract, "period_from" .= iso8601Show srFrom
          , "status" .= (if ctAutoPost ct then "POSTED" else "DRAFT" :: Text), "draft_id" .= draftId
          , "document_id" .= docId, "document_number" .= docNo
          ]
    pure (draftId, docNo)
  case r of
    Left (PluginError code m) -> pluginError code m
    Right (draftId, docNo) ->
      pure $
        object
          [ "message"
              .= ( maybe ("Abrechnung vorerfasst (" <> draftId <> ") – im Hauptbuch buchen") ("Abrechnung gebucht: Beleg " <>) docNo
                    <> "; " <> summary cs <> "; " <> (if diff >= 0 then "Nachzahlung " else "Guthaben ") <> formatAmount dec (abs diff)
                 )
          , "check_result" .= summary cs
          , "draft_id" .= draftId
          , "document_number" .= docNo
          , "table" .= checksJson dec cs
          ]
  where
    shortAcc a = case T.breakOnEnd "-" a of
      ("", _) -> a
      (_, n) -> n
    strField k v = case v of
      Object o | Just (String s) <- KM.lookup (K.fromText k) o -> pure s
      _ -> pluginError Internal ("Antwort ohne " <> k)
