-- | Planung der Sollstellung: Welche Fälligkeiten sind bis zum Stichtag noch
-- nicht gebucht, und welche Belege entstehen daraus? Reine Funktion; das
-- Lesen der Daten und das Buchen macht "Billing.Run".
--
-- Ein Beleg je Vertrag und Fälligkeitstag. Je Fälligkeit zwei Positionen:
--
--   * Forderung (wir erhalten): Soll Abstimmkonto der Vertragsart (Debitor),
--     Haben Sachkonto der Kondition (Erlös).
--   * Verbindlichkeit (wir zahlen): Soll Sachkonto der Kondition (Aufwand),
--     Haben Abstimmkonto (Kreditor).
--
-- Negative Beträge (z. B. Minderungen) tauschen Soll und Haben. Kontierungen
-- (Vertrag, Mietobjekt, Debitor/Kreditor) setzt die Planung nur, wo die
-- Feldstatusgruppe des Kontos sie zulässt; Pflichtfelder prüft der Ledger.
module Billing.Plan
  ( Input (..)
  , Key
  , Entry (..)
  , Line (..)
  , Doc (..)
  , plan
  , entryKey
  , formatAmount
  , formatDay
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar

import Billing.Data
import Billing.Schedule

-- | Bereits gebuchte Fälligkeit: Vertrag, Konditionsart, Objekt, Beginn.
type Key = (Text, Text, Text, Day)

data Input = Input
  { inTypes :: Map Text ContractType
  , inContracts :: [Contract]
  , inConditions :: [Condition]
  , inObjects :: [ContractObject]
  , inMeasurements :: [Measurement]
  , inAccounts :: Map Text Account
  , inFieldStatus :: FieldStatus
  , inPosted :: Set Key
  }

data Entry = Entry
  { eCondition :: Condition
  , eDue :: Due
  , eRentObject :: Maybe Text
  , eAccount :: Text -- ^ Sachkonto der Kondition
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

entryKey :: Entry -> Key
entryKey e = (kContract (eCondition e), kType (eCondition e), kObject (eCondition e), dueFrom (eDue e))

-- | Belege bis zum Stichtag und Meldungen (Vertrag, Text) für alles, was sich
-- nicht buchen lässt.
plan :: Day -> Input -> ([Doc], [(Text, Text)])
plan until Input {..} = (concat docs, concat msgs)
  where
    (docs, msgs) = unzip (map forContract (filter billable inContracts))
    billable c = cStatus c `elem` ["ACTIVE", "TERMINATED"]
    conditionsOf = Map.fromListWith (flip (<>)) [(kContract k, [k]) | k <- inConditions]
    objectsOf = Map.fromListWith (flip (<>)) [(oContract o, [o]) | o <- inObjects]

    forContract c = case Map.lookup (cType c) inTypes of
      Nothing -> ([], [(cId c, "Vertragsart " <> cType c <> " fehlt")])
      Just ct -> case ctReconciliation ct of
        Nothing -> ([], [(cId c, "Vertragsart " <> cType c <> ": Abstimmkonto fehlt (Verträge → Vertragsarten)")])
        Just recon ->
          let results = map (entriesFor c) (Map.findWithDefault [] (cId c) conditionsOf)
              entries = concat [es | Right es <- results]
              errors = [(cId c, e) | Left e <- results]
              byDate = Map.fromListWith (flip (<>)) [(dueDate (eDue e), [e]) | e <- entries]
              payable = ctDirection ct == "PAYABLE"
              docType = maybe (if payable then "KR" else "DR") id (ctDocumentType ct)
              mk (date, es) = Doc c date docType es (concatMap (linesFor c payable recon) es)
           in (map mk (Map.toList byDate), errors)

    entriesFor c k = do
      acc <- maybe (Left ("Kondition " <> kType k <> ": Sachkonto fehlt (Kontenfindung)")) Right (kAccount k)
      let from = max (kFrom k) (cFrom c)
          to = min (kTo k) (cTo c)
          valueAt d = measurementAt (kObject k) (maybe "" id (kMeasurement k)) d
          amountAt d
            | kPerUnit k = maybe 0 (\v -> roundScientific (fromInteger (kAmount k) * v)) (valueAt d)
            | otherwise = kAmount k
          ds = dues (kFrequency k) (kMode k) (kDueDay k) amountAt from to until
          missing = [dueFrom x | kPerUnit k, x <- ds, isNothing (valueAt (dueFrom x))]
          entry x = Entry k x (rentObject c k (dueFrom x)) acc
          open = [e | e <- map entry ds, entryKey e `Set.notMember` inPosted, dueAmount (eDue e) /= 0]
      case missing of
        d : _ ->
          Left
            ( "Kondition " <> kType k <> ": keine Bemessung " <> maybe "?" id (kMeasurement k)
                <> " für " <> kObject k <> " am " <> formatDay d
            )
        [] -> Right open

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
          txt = itemText k due
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

shortNumber :: Text -> Text
shortNumber a = case T.breakOnEnd "-" a of
  ("", _) -> a
  (_, n) -> n

-- | „KM 10/2026“, bei Teilperioden mit Zeitraum.
itemText :: Condition -> Due -> Text
itemText k d =
  let (y, m, _) = toGregorian (duePeriodFrom d)
      period = case kFrequency k of
        Once -> formatDay (dueFrom d)
        Monthly -> T.pack (show m) <> "/" <> T.pack (show y)
        _ -> formatDay (duePeriodFrom d) <> "–" <> formatDay (duePeriodTo d)
      partial = dueFrom d /= duePeriodFrom d || dueTo d /= duePeriodTo d
   in kType k <> " " <> period <> (if partial && kFrequency k /= Once then " anteilig " <> formatDay (dueFrom d) <> "–" <> formatDay (dueTo d) else "")

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
