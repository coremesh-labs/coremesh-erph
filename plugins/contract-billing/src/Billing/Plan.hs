-- | Planung der Sollstellung: Welche Fälligkeiten sind bis zum Stichtag noch
-- nicht gebucht, was hat sich an schon Gebuchtem geändert, und welche Belege
-- entstehen daraus? Reine Funktion; das Lesen der Daten und das Buchen macht
-- "Billing.Run".
--
-- Abgleich je Gruppe (Vertrag, Konditionsart, Objekt, Kalenderperiode):
--
--   * Gruppe ohne Vermerk, fällig bis zum Stichtag: Sollstellung zur
--     Fälligkeit. Ist sie schon vorbei und liegt nicht nach der letzten schon
--     gebuchten des Vertrags (Kondition rückwirkend erfasst, z. B. eine Minderung), ist es
--     eine Nachberechnung zum Datum des Laufs – die alte Periode ist oft
--     schon gesperrt.
--   * Gruppe mit Vermerken: neu rechnen; weicht die Summe vom Vermerkten ab
--     (rückwirkende Anpassung, Minderung, Kündigung, Korrektur), wird die
--     Differenz nachberechnet. Auch offene Vorerfassungen zählen als vermerkt.
--
-- Ein Beleg je Vertrag und Buchungsdatum. Je Fälligkeit zwei Positionen:
--
--   * Forderung (wir erhalten): Soll Abstimmkonto des Zahlers (Debitor),
--     Haben Sachkonto der Kondition (Erlös).
--   * Verbindlichkeit (wir zahlen): Soll Sachkonto der Kondition (Aufwand),
--     Haben Abstimmkonto (Kreditor).
--
-- Das Abstimmkonto steht in den Buchungskreisdaten des Zahlers (Vertragspartner
-- oder abweichender Zahler der Kondition) in der Rolle der Vertragsart. Fehlen
-- sie oder ist der Partner gesperrt, wird der Vertrag nicht gebucht (Meldung).
--
-- Negative Beträge (Minderungen, Gutschriften) tauschen Soll und Haben; ist
-- ein Beleg per Saldo negativ, bekommt er die Gutschrift-Belegart (DG bzw.
-- KG). Kontierungen (Vertrag, Mietobjekt, Debitor/Kreditor) setzt die
-- Planung nur, wo die Feldstatusgruppe des Kontos sie zulässt; Pflichtfelder
-- prüft der Ledger.
module Billing.Plan
  ( Input (..)
  , Group
  , Entry (..)
  , Line (..)
  , Doc (..)
  , plan
  , entryKey
  , docCorrections
  , formatAmount
  , formatDay
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar

import Billing.Data
import Billing.Depreciation
import Billing.Loan
import Billing.Pricing
import Billing.Schedule

-- | Abgleich: Vertrag, Konditionsart, Objekt, Position, Beginn der Kalenderperiode.
type Group = (Text, Text, Text, Integer, Day)

data Input = Input
  { inTypes :: Map Text ContractType
  , inContracts :: [Contract]
  , inConditions :: [Condition]
  , inObjects :: [ContractObject]
  , inMeasurements :: [Measurement]
  , inAccounts :: Map Text Account
  , inFieldStatus :: FieldStatus
  , inLoans :: [LoanTerms] -- ^ Darlehenskonditionen (Zeitscheiben)
  , inLoanPayments :: [LoanPayment] -- ^ Sondertilgungen
  , inDepreciations :: [DepTerms] -- ^ Abschreibungskonditionen (Zeitscheiben)
  , inPartnerAccounts :: Map (Text, Text) PartnerAccount -- ^ (Partner, Rolle) → Buchungskreisdaten im Buchungskreis
  , inPosted :: [Posted] -- ^ schon vermerkte Sollstellungen (vorerfasst oder gebucht)
  , inCorrectionDate :: Day -- ^ Buchungsdatum der Nachberechnungen (Tag des Laufs)
  , inBackdateFrom :: Maybe Day
  , inConditionTypes :: Map Text CondType -- ^ seit 0.13.0: Steuerkennzeichen und Rundungsregel je Konditionsart
  , inDependentTypes :: Map Text Dependent -- ^ nachgelagerte Konditionsarten
  , inTaxCodes :: Map Text TaxCode
  , inRoundings :: Map Text Rounding -- ^ Rundungsregeln (Hauptbuch)
  , inCompanyRounding :: Maybe Text -- ^ Rundungsregel des Buchungskreises
  , inDecimals :: Map Text Int -- ^ Nachkommastellen je Währung
  , inRentBuildings :: Map Text Text -- ^ seit 0.18.0: Mietobjekt → Gebäude
  , inObjectFields :: [(Text, Text)] -- ^ seit 0.18.0: Objektart → Kontierungsspalte (Building, BusinessEntity)
  -- ^ Buchungskreis „rückwirkend buchen ab“ (LedgerCompanyConfig.backdate_from): Nachberechnungen
  -- buchen dann zu ihrer Fälligkeit, frühestens zu diesem Tag, höchstens zum Tag des Laufs
  }

data Entry = Entry
  { eCondition :: Condition
  , eDue :: Due -- ^ Nachberechnung: ganze Periode, Fälligkeit = Tag des Laufs, Betrag = Differenz
  , eRentObject :: Maybe Text
  , eAccount :: Text -- ^ Sachkonto der Kondition
  , eKind :: PostingKind
  , eCounter :: Maybe Text -- ^ Gegenkonto statt Partnerkonto (Abschreibung: Anlagekonto) – Beleg nur auf Sachkonten
  }
  deriving stock (Show)

data Line = Line
  { lAccount :: Text
  , lSide :: Text -- ^ S | H
  , lAmount :: Integer
  , lText :: Text
  , lFields :: [(Text, Text)] -- ^ Kontierungen
  }
  deriving stock (Show, Eq)

data Doc = Doc
  { dContract :: Contract
  , dDate :: Day
  , dDocumentType :: Text
  , dEntries :: [Entry]
  , dLines :: [Line]
  }
  deriving stock (Show)

-- | Schlüssel einer Fälligkeit (Überschneidung von Zeitscheiben).
entryKey :: Entry -> (Text, Text, Text, Integer, Day)
entryKey e = (kContract (eCondition e), kType (eCondition e), kObject (eCondition e), kPos (eCondition e), dueFrom (eDue e))

groupOf :: Entry -> Group
groupOf e = (kContract (eCondition e), kType (eCondition e), kObject (eCondition e), kPos (eCondition e), duePeriodFrom (eDue e))

docCorrections :: Doc -> Int
docCorrections d = length [() | e <- dEntries d, eKind e == Correction]

-- | Belege bis zum Stichtag und Meldungen (Vertrag, Text) für alles, was sich
-- nicht buchen lässt.
plan :: Day -> Input -> ([Doc], [(Text, Text)])
plan until Input {..} = (concat docs, concat msgs)
  where
    (docs, msgs) = unzip (map forContract (filter billable inContracts))
    billable c = cStatus c `elem` ["ACTIVE", "TERMINATED"]
    conditionsOf = Map.fromListWith (flip (<>)) [(kContract k, [k]) | k <- inConditions]
    objectsOf = Map.fromListWith (flip (<>)) [(oContract o, [o]) | o <- inObjects]
    postedOf = Map.fromListWith (flip (<>)) [(pContract p, [p]) | p <- inPosted]
    loansOf = Map.fromListWith (flip (<>)) [(lnContract l, [l]) | l <- inLoans]
    paymentsOf = Map.fromListWith (flip (<>)) [(lpContract p, [p]) | p <- inLoanPayments]
    depsOf = Map.fromListWith (flip (<>)) [(dpContract d, [roundingOf' d]) | d <- inDepreciations]
    roundingOf' d = depRounding inRoundings inConditionTypes (maybe 2 decimalsOf (listToMaybe [c | c <- inContracts, cId c == dpContract d])) d

    forContract c = case Map.lookup (cType c) inTypes of
      Nothing -> ([], [(cId c, "Vertragsart " <> cType c <> " fehlt")])
      Just ct ->
        let conds0 = Map.findWithDefault [] (cId c) conditionsOf
            -- Abstimmkonto je Zahler (Vertragspartner oder abweichender Zahler) aus seinen Buchungskreisdaten in der Rolle der Vertragsart
            -- Nur mit Konditionen oder Darlehen gibt es Partnerzeilen; reine Abschreibung bucht auf Sachkonten.
            needsPartner = not (null conds0) || Map.member (cId c) loansOf
            payers = if needsPartner then nubOrd (cPartner c : [p | k <- conds0, Just p <- [kPayer k]]) else []
            accountOf p = case Map.lookup (p, ctRole ct) inPartnerAccounts of
              Nothing -> Left ("Partnerkonto fehlt: " <> p <> " hat in der Rolle " <> ctRole ct <> " keine Buchungskreisdaten (Partnermodul) – Vertrag nicht gebucht")
              Just a
                | paPostingBlock a -> Left ("Buchungssperre: " <> p <> " in der Rolle " <> ctRole ct <> " – Vertrag nicht gebucht")
                | otherwise -> maybe (Left ("Partnerkonto fehlt: " <> p <> " hat in der Rolle " <> ctRole ct <> " kein Abstimmkonto – Vertrag nicht gebucht")) Right (paAccount a)
            accounts = [(p, accountOf p) | p <- payers]
            problems' = [(cId c, e) | (_, Left e) <- accounts]
            reconOf = Map.fromList [(p, a) | (p, Right a) <- accounts]
         in if not (null problems')
              then ([], problems')
              else
          let conds = conds0
              postings = Map.findWithDefault [] (cId c) postedOf
              -- Auch Perioden rechnen, die schon vermerkt sind, aber (z. B. nach
              -- geändertem Fälligkeitstag) erst nach dem Stichtag fällig wären.
              horizon = maximum (until : [addDays 400 (pTo p) | p <- postings])
              results = [(k, entriesFor c k horizon) | k <- conds, isNothing (kDependent k)]
              -- nachgelagerte Konditionen aus den Fälligkeiten der anderen (Anteil: aus der Basis herausgerechnet)
              (baseEntries, dependentEs, depErrors) = dependents c [k | k <- conds, isJust (kDependent k)] (concat [es | (_, Right es) <- results])
              own = baseEntries <> loanEntries c horizon <> depEntries c horizon
              candidates = own <> dependentEs
              failed = Set.fromList ([(kType k, kObject k, kPos k) | (k, Left _) <- results] <> [(kType k, kObject k, kPos k) | (k, _) <- depErrors])
              -- Gleiche Fälligkeit zweimal = Zeitscheiben einer Kondition überschneiden sich (Datenfehler).
              clashes = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(entryKey e, 1) | e <- own]))
              overlap = [(cId c, "Kondition " <> t <> ": Zeitscheiben überschneiden sich (" <> formatDay from <> ") – Vertrag nicht gebucht") | (_, t, _, _, from) <- clashes]
              errors = [(cId c, e) | (_, Left e) <- results] <> [(cId c, e) | (_, e) <- depErrors] <> overlap
              entries = if null clashes then reconcile c conds postings failed candidates else []
              byDate = Map.fromListWith (flip (<>)) [(dueDate (eDue e), [e]) | e <- entries]
              payable = ctDirection ct == "PAYABLE"
              docType es
                | all (isJust . eCounter) es = "SA"
                | sum (map (dueAmount . eDue) es) < 0 = maybe (if payable then "KG" else "DG") id (ctCreditDocumentType ct)
                | otherwise = maybe (if payable then "KR" else "DR") id (ctDocumentType ct)
              recon e = Map.findWithDefault "" (maybe (cPartner c) id (kPayer (eCondition e))) reconOf
              mk (date, es) = Doc c date (docType es) es (concatMap (\e -> linesFor c payable (recon e) e) es)
           in (map mk (Map.toList byDate), errors)

    -- Abgleich der gerechneten Fälligkeiten mit dem Vermerkten.
    reconcile c conds postings failed candidates =
      let computed = Map.fromListWith (flip (<>)) [(groupOf e, [e]) | e <- candidates]
          postedSum = Map.fromListWith (+) [(postedGroup conds p, pAmount p) | p <- postings]
          lastDue = maximumMaybe [pDue p | p <- postings, pKind p == Original]
          -- schon fällig und nicht nach der letzten gebuchten Fälligkeit: nachträglich
          late e = dueDate (eDue e) < inCorrectionDate && maybe False (dueDate (eDue e) <=) lastDue
          originals =
            [ if late e then asCorrection e else e
            | (g, es) <- Map.toList computed
            , g `Map.notMember` postedSum
            , e <- es
            , dueDate (eDue e) <= until
            , dueAmount (eDue e) /= 0
            ]
          corrections =
            [ correction c g es (filter ((== g) . postedGroup conds) postings) diff
            | (g@(_, t, o, n, _), done) <- Map.toList postedSum
            , (t, o, n) `Set.notMember` failed
            , let es = Map.findWithDefault [] g computed
                  diff = sum (map (dueAmount . eDue) es) - done
            , diff /= 0
            ]
       in originals <> corrections

    asCorrection e = e {eDue = (eDue e) {dueDate = correctionDay (dueDate (eDue e))}, eKind = Correction}

    -- Buchungsdatum einer Nachberechnung: Tag des Laufs; mit „rückwirkend buchen ab“
    -- die ursprüngliche Fälligkeit (nicht vor der Grenze, nicht nach dem Lauf).
    correctionDay due = maybe inCorrectionDate (\b -> min inCorrectionDate (max b due)) inBackdateFrom

    -- Nachberechnung einer Periode: Differenz zum Vermerkten, zum Tag des Laufs.
    -- Konto und Mietobjekt wie gerechnet, sonst wie zuletzt vermerkt (Kondition
    -- entfallen, Vertrag rückwirkend beendet).
    correction c (_, t, o, n, ps) es ps' diff =
      let lastPosted = listToMaybe (reverse (sortOn pDue ps'))
          pe = case es of
            e : _ -> duePeriodTo (eDue e)
            [] -> maybe ps pTo lastPosted
          k = case es of
            e : _ -> eCondition e
            [] -> orphan c t o n ps pe
       in Entry
            { eCondition = k
            , eDue = Due ps pe ps pe (correctionDay (maybe ps (dueDate . eDue) (listToMaybe es))) diff
            , eRentObject = case es of
                e : _ -> eRentObject e
                [] -> lastPosted >>= pRentObject
            , eAccount = case es of
                e : _ -> eAccount e
                [] -> maybe "" pAccount lastPosted
            , eKind = Correction
            , eCounter = case es of
                e : _ -> eCounter e
                [] -> Nothing
            }

    -- Darlehen: Zeilen des Tilgungsplans als Fälligkeiten – Zins aufs Zinskonto,
    -- Tilgung und Sondertilgung aufs Darlehenskonto, Auszahlung (negativ:
    -- Seiten getauscht) aufs Darlehenskonto; Konten und Konditionsarten aus der
    -- Zeitscheibe am Fälligkeitstag.
    loanEntries c horizon = case sortOn lnFrom (Map.findWithDefault [] (cId c) loansOf) of
      [] -> []
      sorted@(first : _) ->
        let termsAt d = last (first : [s | s <- sorted, lnFrom s <= d])
            rows = schedule sorted (Map.findWithDefault [] (cId c) paymentsOf) (cTo c) horizon
            mk typ acc freq row amt =
              let k = loanCondition c typ acc freq row
                  due = Due (rFrom row) (rTo row) (rFrom row) (rTo row) (rDue row) amt
               in Entry k due (rentObject c k (rDue row)) acc Original Nothing
         in concat
              [ case rKind row of
                  Disbursement -> [mk (lnDisbursementType t) (lnLoanAccount t) Once row (negate (rPrincipal row))]
                  Special -> [mk (lnSpecialType t) (lnLoanAccount t) Once row (rPrincipal row)]
                  Regular ->
                    [ mk (lnInterestType t) (lnInterestAccount t) Monthly row (rInterest row)
                    , mk (lnPrincipalType t) (lnLoanAccount t) Monthly row (rPrincipal row)
                    ]
              | row <- rows
              , let t = termsAt (rDue row)
              ]

    -- Abschreibung: je Rate (Raten im Jahr) eine Fälligkeit zum Perioden- bzw.
    -- Vertragsende, Aufwandskonto an Anlagekonto (Vermerk unter der
    -- Konditionsart der Kondition); erst ab „Sollstellung ab“ (davor im Altsystem).
    depEntries c horizon =
      [ Entry k due (rentObject c k (drDue row)) (dpExpenseAccount t) Original (Just (dpAssetAccount t))
      | let (hy, _, _) = toGregorian horizon -- Jahreszeile rechnen, Raten bis horizon
      , row <- concatMap depInstallments (depSchedule (Map.findWithDefault [] (cId c) depsOf) (cTo c) (fromGregorian hy 12 31))
      , drDue row <= horizon
      , drAmount row /= 0
      , drDue row >= maybe (cFrom c) id (cBillingStart c)
      , let t = drTerms row
            freq = case dpInstallments t of
              12 -> Monthly
              6 -> Bimonthly
              4 -> Quarterly
              2 -> HalfYearly
              _ -> Yearly
            k = (loanCondition c (dpType t) (dpExpenseAccount t) freq row') {kFrom = drFrom row, kTo = drTo row}
            row' = Row Regular (drFrom row) (drTo row) (drDue row) 0 0 0
            due = Due (drFrom row) (drTo row) (drFrom row) (drTo row) (drDue row) (drAmount row)
      ]

    loanCondition c typ acc freq row =
      Condition
        { kContract = cId c, kType = typ, kObject = "", kPos = 1, kPerUnit = False, kAmount = 0, kMeasurement = Nothing
        , kFrequency = freq, kDueDay = 1, kStartMonth = 1, kDueMonth = 1, kDueDate = Nothing, kInstallments = 1, kMode = InAdvance, kAccount = Just acc
        , kPayer = Nothing, kFrom = rFrom row, kTo = rTo row, kUnitPrice = Nothing, kDependent = Nothing, kRounding = Nothing
        }

    -- Kondition, die es für die Periode nicht mehr gibt.
    orphan c t o n ps pe =
      Condition
        { kContract = cId c, kType = t, kObject = o, kPos = n, kPerUnit = False, kAmount = 0, kMeasurement = Nothing
        , kFrequency = Monthly, kDueDay = 1, kStartMonth = 1, kDueMonth = 1, kDueDate = Nothing, kInstallments = 1, kMode = InAdvance, kAccount = Nothing, kPayer = Nothing
        , kFrom = ps, kTo = pe, kUnitPrice = Nothing, kDependent = Nothing, kRounding = Nothing
        }

    -- Kalenderperiode eines Vermerks; bei alten Vermerken (ohne Periode) aus
    -- dem Rhythmus der Kondition, die den Tag abdeckt.
    postedGroup conds p =
      let start = case pPeriod p of
            Just d -> d
            Nothing ->
              case [k | k <- conds, kType k == pType p, kObject k == pObject p, kPos k == pPos p, kFrom k <= pFrom p, pFrom p <= kTo k] of
                k : _ -> fst (periodFrom (kStartMonth k) (kFrequency k) (pFrom p))
                [] -> pFrom p
       in (pContract p, pType p, pObject p, pPos p, start)

    entriesFor c k horizon = do
      acc <- maybe (Left ("Kondition " <> kType k <> ": Sachkonto fehlt (Kontenfindung)")) Right (kAccount k)
      -- Perioden ab dem echten Beginn; „Sollstellung ab“ (Übernahme aus einem
      -- Altsystem) übernimmt die Fälligkeiten ab diesem Tag – je volle Periode.
      -- Früher Fälliges ist im Altsystem erledigt (z. B. Versicherungsjahr
      -- Okt–Sep, im September bezahlt: keine anteilige Periode ab Januar).
      let from = max (kFrom k) (cFrom c)
          billedFrom = maybe from id (cBillingStart c)
          to = min (kTo k) (cTo c)
          valueAt d = measurementAt (perUnitObject c k d) (maybe "" id (kMeasurement k)) d
          dec = decimalsOf c
          -- je Einheit: Einzelpreis (genau) × Bemessung nach der Rundungsregel
          amountAt d
            | kPerUnit k = case kUnitPrice k >>= parseDecimal of
                Just p -> maybe 0 (\v -> roundMinor (roundingOf c k Nothing) dec (p * toRational v)) (valueAt d)
                Nothing -> maybe 0 (\v -> roundScientific (fromInteger (kAmount k) * v)) (valueAt d)
            | otherwise = kAmount k
          ds = filter ((>= billedFrom) . dueDate) $ case kFrequency k of
            Once -> dueOnce (kInstallments k) (kDueDate k) amountAt from to horizon
            f -> duesFrom (kStartMonth k) (kDueMonth k) f (kMode k) (kDueDay k) amountAt from to horizon
          missing = [dueFrom x | kPerUnit k, x <- ds, dueDate x <= until, isNothing (valueAt (dueFrom x))]
      case missing of
        d : _ ->
          Left
            ( "Kondition " <> kType k <> ": keine Bemessung " <> maybe "?" id (kMeasurement k)
                <> " für " <> perUnitObject c k d <> " am " <> formatDay d
            )
        [] -> Right [Entry k x (rentObject c k (dueFrom x)) acc Original Nothing | x <- ds]

    decimalsOf c = Map.findWithDefault 2 (cCurrency c) inDecimals

    -- Rundungsregel: Kondition, (Art der nachgelagerten Kondition,) Konditionsart,
    -- Buchungskreis, Standardregel, kaufmännisch.
    roundingOf c k depRule =
      let pick code = Map.lookup code inRoundings >>= \r -> if rActive r then Just r else Nothing
          typeRule = Map.lookup (kType k) inConditionTypes >>= ktRounding
          chain = [x | Just x <- [kRounding k, depRule, typeRule, inCompanyRounding]]
          defaults' = [r | r <- sortOn rCode (Map.elems inRoundings), rDefault r, rActive r]
       in case [r | Just r <- map pick chain] <> defaults' of
            r : _ -> r
            [] -> builtinRounding (decimalsOf c)

    -- Nachgelagerte Konditionen eines Vertrags der Reihe nach (Reihenfolge, Schlüssel der
    -- Art). Basis: Fälligkeiten der anderen Konditionen im Zeitraum der nachgelagerten,
    -- je Fälligkeit (Datum, Zeitraum) und Steuerkennzeichen. Zuschlag: auf die Summe,
    -- kommt als Fälligkeit dazu (und ist Basis späterer Arten, außer Steuer auf
    -- Steuer); Anteil: je Basisfälligkeit gerundet und dort abgezogen.
    -- Liefert die Basisfälligkeiten (Anteile abgezogen), die neuen und die Fehler.
    dependents c dconds base =
      let defs =
            sortOn
              (\(_, d') -> (dSort d', dCode d'))
              [(k, d') | k <- dconds, Just code <- [kDependent k], Just d' <- [Map.lookup code inDependentTypes]]
          missing = [(k, "Kondition " <> kType k <> ": nachgelagerte Konditionsart " <> code <> " fehlt") | k <- dconds, Just code <- [kDependent k], Map.notMember code inDependentTypes]
          taxOfType t = Map.lookup t inConditionTypes >>= ktTaxCode
          -- Pool: (Index der Basisfälligkeit, Fälligkeit, Steuerkennzeichen, Ergebnis einer Steuer)
          pool0 = [(Just i, e, taxOfType (kType (eCondition e)), False) | (i, e) <- zip [0 :: Int ..] base]
          dec = decimalsOf c
          dueKey x = (dueDate x, dueFrom x, dueTo x, duePeriodFrom x, duePeriodTo x)
          step (pool, outs, shares, errs) (k, d') =
            let isTaxD = dMethod d' == "TAX"
                rule = roundingOf c k (dRounding d')
                inBase (_, e, _, isTax) =
                  let t = kType (eCondition e)
                      f = dueFrom (eDue e)
                   in kFrom k <= f && f <= kTo k
                        && (if null (dBase d') then not isTax else t `elem` dBase d' && not (isTax && isTaxD))
                group (_, e, tax, _) = (dueKey (eDue e), if isTaxD then maybe tax Just (dTaxCode d') else tax)
                matched = [p | p@(_, _, _, _) <- pool, inBase p, not isTaxD || isJust (snd (group p))]
                groups = Map.toList (Map.fromListWith (flip (<>)) [(group p, [p]) | p <- matched])
                rateOf tax
                  | isTaxD = case tax >>= (`Map.lookup` inTaxCodes) of
                      Just tc -> Right (tcRate tc, tcAccount tc)
                      Nothing -> Left ("Kondition " <> kType k <> ": Steuerkennzeichen " <> maybe "?" id tax <> " fehlt (Hauptbuch)")
                  | otherwise = maybe (Left ("Kondition " <> kType k <> ": Prozentsatz der Art " <> dCode d' <> " fehlt")) (\p -> Right (p, Nothing)) (dPercent d')
                major x = fromInteger x / fromInteger (10 ^ dec) :: Rational
                one ((_, tax), ps@((_, e0, _, _) : _)) = do
                  (rate, taxAcc) <- rateOf tax
                  acc <- maybe (Left ("Kondition " <> kType k <> ": Sachkonto fehlt (nachgelagerte Art " <> dCode d' <> ")")) Right
                           (maybe (maybe taxAcc Just (dAccount d')) Just (kAccount k))
                  let f = rate / 100
                      perEntry = [(i, roundMinor rule dec (major (dueAmount (eDue e)) * f)) | (i, e, _, _) <- ps]
                      amount
                        | dEffect d' == "SHARE" = sum (map snd perEntry)
                        | otherwise = roundMinor rule dec (major (sum [dueAmount (eDue e) | (_, e, _, _) <- ps]) * f)
                      due = (eDue e0) {dueAmount = amount}
                      entry = Entry k {kAccount = Just acc} due (rentObject c k (dueFrom due)) acc Original Nothing
                      sh = if dEffect d' == "SHARE" then [(i', s) | (Just i', s) <- perEntry] else []
                  pure (entry, tax, sh)
                one (_, []) = Left "leer"
                computed = map one groups
                errs' = [(k, e) | Left e <- computed]
                made = [x | Right x@(e, _, _) <- computed, dueAmount (eDue e) /= 0]
                pool' = pool <> [(Nothing, e, tax, isTaxD) | dEffect d' /= "SHARE", (e, tax, _) <- made]
             in if null errs'
                  then (pool', outs <> [e | (e, _, _) <- made], shares <> concat [s | (_, _, s) <- made], errs)
                  else (pool, outs, shares, errs <> take 1 errs')
          (_, outs, shareList, errs) = foldl' step (pool0, [], [], []) defs
          shareOf = Map.fromListWith (+) shareList
          base' = [e {eDue = (eDue e) {dueAmount = dueAmount (eDue e) - Map.findWithDefault 0 i shareOf}} | (i, e) <- zip [0 ..] base]
       in (base', outs, missing <> errs)

    measurementAt obj typ d =
      listToMaybe [mValue m | m <- inMeasurements, mObject m == obj, mType m == typ, mFrom m <= d, d <= mTo m]

    -- Objekt der Bemessung (je Einheit): das der Kondition, sonst (seit 0.16.0) das Hauptobjekt
    -- des Vertrags am Stichtag.
    perUnitObject c k d
      | T.null (kObject k) =
          maybe "" oId (listToMaybe (sortOn (not . oMain) [o | o <- Map.findWithDefault [] (cId c) objectsOf, oFrom o <= d, d <= oTo o]))
      | otherwise = kObject k

    -- Mietobjekt der Kontierung: das Objekt der Kondition, sonst das Hauptobjekt
    -- des Vertrags am Stichtag – jeweils nur, wenn es ein Mietobjekt ist.
    rentObject c k d =
      let objs = Map.findWithDefault [] (cId c) objectsOf
          valid o = oFrom o <= d && d <= oTo o
          isRent o = oType o == "RentObject"
       in if T.null (kObject k)
            then oId <$> listToMaybe (sortOn (not . oMain) [o | o <- objs, valid o, oMain o, isRent o])
            else case [o | o <- objs, oId o == kObject k] of
              o : _ | isRent o -> Just (oId o)
              [] -> Just (kObject k) -- Objekt ohne Zuordnung: wie erfasst
              _ -> Nothing

    linesFor c payable recon e =
      let k = eCondition e
          due = eDue e
          amount = dueAmount due
          (debit, credit) = if amount < 0 then ("H", "S") else ("S", "H")
          partner = maybe (cPartner c) id (kPayer k)
          txt = case eKind e of
            Original -> itemText k due
            Correction -> "Nachberechnung " <> kType k <> " " <> periodText (dueFrom due) (dueTo due)
          common = [("rent_contract_id", cId c)] <> maybe [] (\o -> [("rent_object_id", o)]) (eRentObject e)
          withFields acc fs = Line acc "" (abs amount) txt (allowed acc fs)
          -- Objekt des Vertrags in die Kontierungsspalten der Einstellung object_fields: das
          -- Hauptobjekt am Fälligkeitstag (sonst das zuletzt gültige Hauptobjekt); ein
          -- Mietobjekt bucht sein Gebäude in die Spalte von Building (seit 0.18.0) – so
          -- sammelt die GuV je Gebäude Mieten, Kosten und Zinsen.
          objectDims =
            [ (field, v)
            | o <- take 1 (mainAt c (dueDate due))
            , (field, v) <-
                [(f, oId o) | Just f <- [lookup (oType o) inObjectFields]]
                  <> [ (f, b)
                     | oType o == "RentObject"
                     , Just b <- [Map.lookup (oId o) inRentBuildings]
                     , Just f <- [lookup "Building" inObjectFields]
                     ]
            ]
       in case eCounter e of
            Just counter ->
              [ (withFields (eAccount e) (common <> objectDims)) {lSide = debit}
              , (withFields counter (common <> objectDims)) {lSide = credit}
              ]
            Nothing
              -- Gebäude bzw. Wirtschaftseinheit des Vertrags auch auf der Aufwands- bzw. Erlöszeile
              -- (z. B. Versorger- oder Hausmeistervertrag für ein Haus)
              | payable ->
                  [ (withFields (eAccount e) (common <> objectDims <> [("supplier_id", partner)])) {lSide = debit}
                  , (withFields recon (common <> [("supplier_id", partner)])) {lSide = credit}
                  ]
              | otherwise ->
                  [ (withFields recon (common <> [("sd_customer_id", partner)])) {lSide = debit}
                  , (withFields (eAccount e) (common <> objectDims)) {lSide = credit}
                  ]

    -- Hauptobjekte des Vertrags: gültig am Tag zuerst, sonst das zuletzt begonnene.
    mainAt c d =
      let mains = [o | o <- Map.findWithDefault [] (cId c) objectsOf, oMain o]
       in [o | o <- mains, oFrom o <= d, d <= oTo o] <> sortOn (Down . oFrom) mains

    -- Nur Kontierungen, die die Feldstatusgruppe des Kontos nicht ausblendet.
    allowed acc fs = case Map.lookup (shortNumber acc) inAccounts of
      Nothing -> fs
      Just a ->
        let grp = if T.null (aGroup a) then "STD" else aGroup a
         in [f | f@(name, _) <- fs, Map.lookup (grp, name) inFieldStatus `elem` [Just "OPTIONAL", Just "REQUIRED"]]

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just (maximum xs)

shortNumber :: Text -> Text
shortNumber a = case T.breakOnEnd "-" a of
  ("", _) -> a
  (_, n) -> n

-- | „KM 10/2026“, bei Teilperioden mit Zeitraum.
itemText :: Condition -> Due -> Text
itemText k d =
  let period = case kFrequency k of
        Once -> formatDay (dueFrom d)
        _ -> periodText (duePeriodFrom d) (duePeriodTo d)
      partial = dueFrom d /= duePeriodFrom d || dueTo d /= duePeriodTo d
   in kType k <> " " <> period <> (if partial && kFrequency k /= Once then " anteilig " <> formatDay (dueFrom d) <> "–" <> formatDay (dueTo d) else "")

-- | Ein Kalendermonat als „10/2026“, sonst „01.10.2026–31.12.2026“ bzw. ein Tag.
periodText :: Day -> Day -> Text
periodText from to
  | from == to = formatDay from
  | d == 1 && to == fromGregorian y m (gregorianMonthLength y m) = T.pack (show m) <> "/" <> T.pack (show y)
  | otherwise = formatDay from <> "–" <> formatDay to
  where
    (y, m, d) = toGregorian from

formatDay :: Day -> Text
formatDay d = let (y, m, dd) = toGregorian d in T.pack (pad dd <> "." <> pad m <> "." <> show y)
  where
    pad n = (if n < 10 then "0" else "") <> show n

-- | Kleinste Einheit → Dezimaltext ("125050", 2 → "1250.50").
formatAmount :: Int -> Integer -> Text
formatAmount decimals minor =
  let sign = if minor < 0 then "-" else ""
      s = show (abs minor)
      padded = replicate (decimals + 1 - length s) '0' <> s
      (whole, frac) = splitAt (length padded - decimals) padded
   in T.pack (sign <> whole <> (if decimals > 0 then "." <> frac else ""))

roundScientific :: Scientific -> Integer
roundScientific x
  | x >= 0 = floor (x + 0.5)
  | otherwise = negate (floor (negate x + 0.5))
