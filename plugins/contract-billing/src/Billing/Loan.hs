-- | Tilgungsplan eines Darlehens – reine Funktionen ohne Ein-/Ausgabe.
--
--   * Zinsperioden enden an den Fälligkeiten (Rhythmus, Fälligkeitstag); die
--     erste beginnt mit der Auszahlung bzw. Übernahme.
--   * Zins je Periode: Restschuld × Satz × Zeitanteil nach Zinsmethode
--     (30/360 deutsch, act/360, act/365), stückweise über Zinssatzwechsel
--     (Zeitscheiben der Konditionen) und Sondertilgungen; je Periode
--     kaufmännisch auf die kleinste Einheit gerundet.
--   * Tilgung an der Fälligkeit: Annuität = Rate − Zins, Ratentilgung = Rate,
--     endfällig = 0; nie mehr als die Restschuld. An der letzten Fälligkeit
--     im Vertrag (Vertragsende) wird die Restschuld getilgt.
--   * Sondertilgungen mindern die Restschuld an ihrem Tag.
module Billing.Loan
  ( DayCount (..)
  , Repayment (..)
  , LoanTerms (..)
  , LoanPayment (..)
  , RowKind (..)
  , Row (..)
  , schedule
  , yearFraction
  , parseRate
  ) where

import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar

import Billing.Schedule (Frequency (..), roundDiv)

data DayCount = D30360 | ACT360 | ACT365
  deriving stock (Eq, Show)

data Repayment = Annuity | Instalment | Bullet
  deriving stock (Eq, Show)

-- | Eine Zeitscheibe der Darlehenskonditionen.
data LoanTerms = LoanTerms
  { lnContract :: Text
  , lnFrom :: Day -- ^ Zeitscheibe gültig ab
  , lnTo :: Day
  , lnPrincipal :: Integer -- ^ kleinste Einheit; maßgeblich ist die erste Zeitscheibe
  , lnStart :: Day -- ^ Auszahlung bzw. Übernahme (Zinsbeginn)
  , lnTakeover :: Bool -- ^ Übernahme mit Anfangsbestand: Auszahlung nicht buchen
  , lnRepayment :: Repayment
  , lnRate :: Rational -- ^ Prozent p. a.
  , lnInstallment :: Integer -- ^ Rate (Annuität bzw. Tilgung); endfällig 0
  , lnFrequency :: Frequency
  , lnDueDay :: Int
  , lnDayCount :: DayCount
  , lnLoanAccount :: Text
  , lnInterestAccount :: Text
  , lnInterestType :: Text
  , lnPrincipalType :: Text
  , lnSpecialType :: Text
  , lnDisbursementType :: Text
  }
  deriving stock (Show)

data LoanPayment = LoanPayment
  { lpContract :: Text
  , lpDate :: Day
  , lpAmount :: Integer
  }
  deriving stock (Show)

data RowKind = Disbursement | Regular | Special
  deriving stock (Eq, Show)

-- | Eine Zeile des Plans.
data Row = Row
  { rKind :: RowKind
  , rFrom :: Day -- ^ Zinsperiode (Regular), sonst der Tag
  , rTo :: Day
  , rDue :: Day
  , rInterest :: Integer
  , rPrincipal :: Integer -- ^ Tilgung (Disbursement: ausgezahlter Betrag)
  , rBalance :: Integer -- ^ Restschuld danach
  }
  deriving stock (Eq, Show)

-- | "3.45" → 69/20.
parseRate :: Text -> Maybe Rational
parseRate t = case T.splitOn "." (T.replace "," "." (T.strip t)) of
  [w] | Just a <- int w -> Just (fromInteger a)
  [w, f] | Just a <- int w, Just b <- int f, not (T.null f) -> Just (fromInteger a + b % (10 ^ T.length f))
  _ -> Nothing
  where
    int x = case reads (T.unpack x) of
      [(n, "")] | n >= 0 -> Just n
      _ -> Nothing

-- | Zeitanteil zwischen zwei Tagen (a ausschließlich, b einschließlich).
yearFraction :: DayCount -> Day -> Day -> Rational
yearFraction dc a b = case dc of
  D30360 ->
    -- deutsche Zinsmethode: jeder Monat 30 Tage, Monatsende (auch Februar) = 30.
    let day30 y m dd = if dd >= gregorianMonthLength y m then 30 else min 30 dd
        (y1, m1, d1) = toGregorian a
        (y2, m2, d2) = toGregorian b
        days = (y2 - y1) * 360 + toInteger (m2 - m1) * 30 + toInteger (day30 y2 m2 d2 - day30 y1 m1 d1)
     in days % 360
  ACT360 -> diffDays b a % 360
  ACT365 -> diffDays b a % 365

months :: Frequency -> Integer
months = \case
  Monthly -> 1
  Quarterly -> 3
  HalfYearly -> 6
  Yearly -> 12
  Once -> 1

dayInMonth :: Integer -> Int -> Int -> Day
dayInMonth y m d = fromGregorian y m (min d (gregorianMonthLength y m))

-- | Fälligkeiten nach dem Beginn: Fälligkeitstag im Rhythmus, ab dem Monat des Beginns.
dueDates :: Frequency -> Int -> Day -> [Day]
dueDates f dd start =
  let (y, m, _) = toGregorian start
      first = fromGregorian y m 1
      ds = [let (yy, mm, _) = toGregorian (addGregorianMonthsClip (k * months f) first) in dayInMonth yy mm dd | k <- [0 ..]]
   in dropWhile (<= start) ds

-- | Plan bis zum Horizont (Fälligkeit ≤ horizon) bzw. bis die Restschuld 0 ist.
-- contractEnd: letzte Fälligkeit davor tilgt die Restschuld (9999-12-31 = unbefristet).
schedule :: [LoanTerms] -> [LoanPayment] -> Day -> Day -> [Row]
schedule [] _ _ _ = []
schedule slices0 payments contractEnd horizon =
  disbursement <> go start principal (take 1200 dues) (sortOn lpDate [p | p <- payments, lpDate p > start])
  where
    slices = sortOn lnFrom slices0
    base = case slices of
      s : _ -> s
      [] -> error "schedule: keine Konditionen"
    start = lnStart base
    principal = lnPrincipal base
    disbursement = [Row Disbursement start start start 0 principal principal | not (lnTakeover base), start <= horizon]
    termsAt d = maybe base id (listToMaybe (reverse [s | s <- slices, lnFrom s <= d]))
    dues = takeWhile (<= contractEnd) (dueDates (lnFrequency base) (lnDueDay base) start) <> lastDue
    lastDue = [contractEnd | contractEnd < fromGregorian 9999 1 1, contractEnd > start]
    go _ _ [] _ = []
    go prev balance (due : rest) pays
      | balance <= 0 || due > horizon = []
      | otherwise =
          let inPeriod = takeWhile ((<= due) . lpDate) pays
              later = drop (length inPeriod) pays
              -- Zins stückweise für die Tage (a, b]: Schnitt an Sondertilgungen (Tag
              -- der Zahlung noch mit der alten Restschuld) und am Vortag eines
              -- Zinssatzwechsels; Satz des ersten Tags im Abschnitt.
              cuts = sortOn id (filter (\d -> d > prev && d < due) (map lpDate inPeriod <> map (addDays (-1) . lnFrom) slices))
              points = prev : cuts <> [due]
              balanceAt d = max 0 (balance - sum [lpAmount p | p <- inPeriod, lpDate p <= d])
              segment a b = let s = termsAt (addDays 1 a) in fromInteger (balanceAt a) * lnRate s / 100 * yearFraction (lnDayCount s) a b
              exact = sum [segment a b | (a, b) <- zip points (drop 1 points)]
              interest = roundRational exact
              specials = specialRows balance inPeriod
              afterSpecials = max 0 (balance - sum (map rPrincipal specials))
              t = termsAt due
              final = null rest || due >= contractEnd
              regular = case lnRepayment t of
                Annuity -> max 0 (lnInstallment t - interest)
                Instalment -> lnInstallment t
                Bullet -> 0
              principalPart = if final then afterSpecials else min afterSpecials regular
              row = Row Regular (addDays 1 prev) due due interest principalPart (afterSpecials - principalPart)
           in specials <> [row] <> go due (afterSpecials - principalPart) rest later
    specialRows _ [] = []
    specialRows bal (p : ps) =
      let amt = min bal (lpAmount p)
       in [Row Special (lpDate p) (lpDate p) (lpDate p) 0 amt (bal - amt) | amt > 0] <> specialRows (bal - amt) ps

-- | Kaufmännisch auf ganze Einheiten.
roundRational :: Rational -> Integer
roundRational x = roundDiv (numerator x) (denominator x)
