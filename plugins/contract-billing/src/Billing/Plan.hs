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
import Data.Maybe (isNothing, listToMaybe)
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar

import Billing.Data
import Billing.Schedule

-- | Abgleich: Vertrag, Konditionsart, Objekt, Beginn der Kalenderperiode.
type Group = (Text, Text, Text, Day)

data Input = Input
  { inTypes :: Map Text ContractType
  , inContracts :: [Contract]
  , inConditions :: [Condition]
  , inObjects :: [ContractObject]
  , inMeasurements :: [Measurement]
  , inAccounts :: Map Text Account
  , inFieldStatus :: FieldStatus
  , inPartnerAccounts :: Map (Text, Text) PartnerAccount -- ^ (Partner, Rolle) → Buchungskreisdaten im Buchungskreis
  , inPosted :: [Posted] -- ^ schon vermerkte Sollstellungen (vorerfasst oder gebucht)
  , inCorrectionDate :: Day -- ^ Buchungsdatum der Nachberechnungen (Tag des Laufs)
  }

data Entry = Entry
  { eCondition :: Condition
  , eDue :: Due -- ^ Nachberechnung: ganze Periode, Fälligkeit = Tag des Laufs, Betrag = Differenz
  , eRentObject :: Maybe Text
  , eAccount :: Text -- ^ Sachkonto der Kondition
  , eKind :: PostingKind
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
entryKey :: Entry -> (Text, Text, Text, Day)
entryKey e = (kContract (eCondition e), kType (eCondition e), kObject (eCondition e), dueFrom (eDue e))

groupOf :: Entry -> Group
groupOf e = (kContract (eCondition e), kType (eCondition e), kObject (eCondition e), duePeriodFrom (eDue e))

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

    forContract c = case Map.lookup (cType c) inTypes of
      Nothing -> ([], [(cId c, "Vertragsart " <> cType c <> " fehlt")])
      Just ct ->
        let conds0 = Map.findWithDefault [] (cId c) conditionsOf
            -- Abstimmkonto je Zahler (Vertragspartner oder abweichender Zahler) aus seinen Buchungskreisdaten in der Rolle der Vertragsart
            payers = nubOrd (cPartner c : [p | k <- conds0, Just p <- [kPayer k]])
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
              results = [(k, entriesFor c k horizon) | k <- conds]
              candidates = concat [es | (_, Right es) <- results]
              failed = Set.fromList [(kType k, kObject k) | (k, Left _) <- results]
              -- Gleiche Fälligkeit zweimal = Zeitscheiben einer Kondition überschneiden sich (Datenfehler).
              clashes = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(entryKey e, 1) | e <- candidates]))
              overlap = [(cId c, "Kondition " <> t <> ": Zeitscheiben überschneiden sich (" <> formatDay from <> ") – Vertrag nicht gebucht") | (_, t, _, from) <- clashes]
              errors = [(cId c, e) | (_, Left e) <- results] <> overlap
              entries = if null clashes then reconcile c conds postings failed candidates else []
              byDate = Map.fromListWith (flip (<>)) [(dueDate (eDue e), [e]) | e <- entries]
              payable = ctDirection ct == "PAYABLE"
              docType es
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
            | (g@(_, t, o, _), done) <- Map.toList postedSum
            , (t, o) `Set.notMember` failed
            , let es = Map.findWithDefault [] g computed
                  diff = sum (map (dueAmount . eDue) es) - done
            , diff /= 0
            ]
       in originals <> corrections

    asCorrection e = e {eDue = (eDue e) {dueDate = inCorrectionDate}, eKind = Correction}

    -- Nachberechnung einer Periode: Differenz zum Vermerkten, zum Tag des Laufs.
    -- Konto und Mietobjekt wie gerechnet, sonst wie zuletzt vermerkt (Kondition
    -- entfallen, Vertrag rückwirkend beendet).
    correction c (_, t, o, ps) es ps' diff =
      let lastPosted = listToMaybe (reverse (sortOn pDue ps'))
          pe = case es of
            e : _ -> duePeriodTo (eDue e)
            [] -> maybe ps pTo lastPosted
          k = case es of
            e : _ -> eCondition e
            [] -> orphan c t o ps pe
       in Entry
            { eCondition = k
            , eDue = Due ps pe ps pe inCorrectionDate diff
            , eRentObject = case es of
                e : _ -> eRentObject e
                [] -> lastPosted >>= pRentObject
            , eAccount = case es of
                e : _ -> eAccount e
                [] -> maybe "" pAccount lastPosted
            , eKind = Correction
            }

    -- Kondition, die es für die Periode nicht mehr gibt.
    orphan c t o ps pe =
      Condition
        { kContract = cId c, kType = t, kObject = o, kPerUnit = False, kAmount = 0, kMeasurement = Nothing
        , kFrequency = Monthly, kDueDay = 1, kDueDate = Nothing, kMode = InAdvance, kAccount = Nothing, kPayer = Nothing
        , kFrom = ps, kTo = pe
        }

    -- Kalenderperiode eines Vermerks; bei alten Vermerken (ohne Periode) aus
    -- dem Rhythmus der Kondition, die den Tag abdeckt.
    postedGroup conds p =
      let start = case pPeriod p of
            Just d -> d
            Nothing ->
              case [k | k <- conds, kType k == pType p, kObject k == pObject p, kFrom k <= pFrom p, pFrom p <= kTo k] of
                k : _ -> fst (periodOf (kFrequency k) (pFrom p))
                [] -> pFrom p
       in (pContract p, pType p, pObject p, start)

    entriesFor c k horizon = do
      acc <- maybe (Left ("Kondition " <> kType k <> ": Sachkonto fehlt (Kontenfindung)")) Right (kAccount k)
      let from = max (kFrom k) (cFrom c)
          to = min (kTo k) (cTo c)
          valueAt d = measurementAt (kObject k) (maybe "" id (kMeasurement k)) d
          amountAt d
            | kPerUnit k = maybe 0 (\v -> roundScientific (fromInteger (kAmount k) * v)) (valueAt d)
            | otherwise = kAmount k
          ds = case kFrequency k of
            Once -> dueOnce (kDueDate k) amountAt from to horizon
            f -> dues f (kMode k) (kDueDay k) amountAt from to horizon
          missing = [dueFrom x | kPerUnit k, x <- ds, dueDate x <= until, isNothing (valueAt (dueFrom x))]
      case missing of
        d : _ ->
          Left
            ( "Kondition " <> kType k <> ": keine Bemessung " <> maybe "?" id (kMeasurement k)
                <> " für " <> kObject k <> " am " <> formatDay d
            )
        [] -> Right [Entry k x (rentObject c k (dueFrom x)) acc Original | x <- ds]

    measurementAt obj typ d =
      listToMaybe [mValue m | m <- inMeasurements, mObject m == obj, mType m == typ, mFrom m <= d, d <= mTo m]

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
       in if payable
            then
              [ (withFields (eAccount e) (common <> [("supplier_id", partner)])) {lSide = debit}
              , (withFields recon (common <> [("supplier_id", partner)])) {lSide = credit}
              ]
            else
              [ (withFields recon (common <> [("sd_customer_id", partner)])) {lSide = debit}
              , (withFields (eAccount e) common) {lSide = credit}
              ]

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
