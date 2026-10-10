-- | Fälligkeiten einer Kondition: Perioden, anteilige Beträge, Fälligkeitstage.
-- Reine Funktionen ohne Ein-/Ausgabe.
--
--   * Perioden sind Kalenderperioden: Monat, zwei Monate, Quartal (ab
--     Jan/Apr/Jul/Okt), Halbjahr (ab Jan/Jul), Jahr – oder ab einem eigenen
--     Startmonat (z. B. Versicherungsjahr Oktober bis September). Einmalig: ein Tag, der Beginn der Kondition;
--     fällig am Beginn oder an einem eigenen Fälligkeitsdatum ("dueOnce").
--   * Deckt die Kondition nur einen Teil einer Periode ab (Beginn am 15.,
--     Anpassung mitten im Monat, Kündigung), ist der Betrag tagesgenau anteilig
--     und kaufmännisch gerundet.
--   * Fälligkeit vorschüssig: Fälligkeitstag im ersten Monat der Periode, nicht
--     vor Beginn des abgedeckten Teils. Nachschüssig: Fälligkeitstag im Monat
--     nach dem Ende der Periode. Ein Fälligkeitstag über dem Monatsende gilt als
--     letzter Tag des Monats.
module Billing.Schedule
  ( Frequency (..)
  , PaymentMode (..)
  , Due (..)
  , dues
  , dueOnce
  , periodOf
  , periodFrom
  , duesFrom
  , prorate
  , roundDiv
  ) where

import Data.Time.Calendar

data Frequency = Monthly | Bimonthly | Quarterly | HalfYearly | Yearly | Once
  deriving stock (Eq, Show, Read)

data PaymentMode = InAdvance | InArrears
  deriving stock (Eq, Show, Read)

-- | Eine fällige Teilleistung.
data Due = Due
  { dueFrom :: Day -- ^ abgedeckter Teil der Periode
  , dueTo :: Day
  , duePeriodFrom :: Day -- ^ ganze Kalenderperiode
  , duePeriodTo :: Day
  , dueDate :: Day
  , dueAmount :: Integer -- ^ kleinste Währungseinheit
  }
  deriving stock (Eq, Show)

-- | Kalenderperiode, die den Tag enthält.
periodOf :: Frequency -> Day -> (Day, Day)
periodOf = periodFrom 1

-- | Periode, die den Tag enthält, mit Periodenbeginn im Monat startMonth
-- (1 = Kalender: Quartal ab Jan/Apr/Jul/Okt; 10 = Versicherungsjahr Okt–Sep).
periodFrom :: Int -> Frequency -> Day -> (Day, Day)
periodFrom startMonth f d =
  let (y, m, _) = toGregorian d
      months = case f of
        Monthly -> 1
        Bimonthly -> 2
        Quarterly -> 3
        HalfYearly -> 6
        Yearly -> 12
        Once -> 1
      base = toInteger ((max 1 (min 12 startMonth) - 1) `mod` months)
      idx = y * 12 + toInteger (m - 1) -- fortlaufende Monatsnummer
      startIdx = ((idx - base) `div` toInteger months) * toInteger months + base
      start = fromGregorian (startIdx `div` 12) (fromInteger (startIdx `mod` 12) + 1) 1
      end = addDays (-1) (addGregorianMonthsClip (toInteger months) start)
   in case f of
        Once -> (d, d)
        _ -> (start, end)

-- | a·z/n, kaufmännisch gerundet (halbe Einheiten von der Null weg).
roundDiv :: Integer -> Integer -> Integer
roundDiv z n
  | n <= 0 = error "roundDiv: Nenner muss positiv sein"
  | z >= 0 = (2 * z + n) `div` (2 * n)
  | otherwise = negate ((2 * negate z + n) `div` (2 * n))

-- | Betrag für den abgedeckten Teil [a, b] der Periode [ps, pe].
prorate :: Integer -> (Day, Day) -> (Day, Day) -> Integer
prorate amount (ps, pe) (a, b)
  | a <= ps && b >= pe = amount
  | otherwise = roundDiv (amount * days a b) (days ps pe)
  where
    days x y = diffDays y x + 1

-- | Tag des Monats, höchstens der letzte.
dayInMonth :: Integer -> Int -> Int -> Day
dayInMonth y m d = fromGregorian y m (min d (gregorianMonthLength y m))

-- | Alle Fälligkeiten der Kondition (gültig von–bis, beide einschließlich) mit
-- Fälligkeitsdatum bis horizon. amountAt liefert den Betrag einer vollen
-- Periode für den abgedeckten Teil (z. B. Preis je m² × Fläche am Beginn).
dues :: Frequency -> PaymentMode -> Int -> (Day -> Integer) -> Day -> Day -> Day -> [Due]
dues = duesFrom 1 1

-- | Wie dues, Perioden ab dem Monat startMonth (siehe periodFrom); vorschüssig
-- fällig im dueMonth-ten Monat der Periode (1 = erster), z. B. Jahresmiete für
-- Jan–Dez, fällig am 15.03.
duesFrom :: Int -> Int -> Frequency -> PaymentMode -> Int -> (Day -> Integer) -> Day -> Day -> Day -> [Due]
duesFrom _ _ Once _ _ amountAt from to horizon
  | from <= to && from <= horizon = [Due from from from from from (amountAt from)]
  | otherwise = []
duesFrom sm dm f mode dueDay amountAt from to horizon = go (fst (periodFrom sm f from))
  where
    go ps
      | ps > to = []
      | date > horizon = [] -- Fälligkeiten steigen mit der Periode
      | otherwise = Due a b ps pe date (prorate (amountAt a) (ps, pe) (a, b)) : go (addDays 1 pe)
      where
        pe = snd (periodFrom sm f ps)
        a = max ps from
        b = min pe to
        date = case mode of
          InAdvance ->
            let (y, m, _) = toGregorian (addGregorianMonthsClip (toInteger (max 1 dm - 1)) ps)
             in max a (min pe (dayInMonth y m dueDay))
          InArrears ->
            let (y, m, _) = toGregorian (addDays 1 pe)
             in dayInMonth y m dueDay

-- | Einmalige Kondition: ein Betrag für den Beginn, fällig am eigenen
-- Fälligkeitsdatum (nicht vor dem Beginn) bzw. am Beginn – in n Monatsraten
-- (z. B. Kaution in drei Raten): jede Rate eine eigene Fälligkeit, ein Monat
-- nach der vorigen; der Rest der Division geht auf die erste Rate. Die erste
-- Rate deckt den Beginn ab (wie eine einmalige Kondition ohne Raten), jede
-- weitere ihren Fälligkeitstag – so bleibt jede Rate eindeutig vermerkt.
dueOnce :: Int -> Maybe Day -> (Day -> Integer) -> Day -> Day -> Day -> [Due]
dueOnce n dueOn amountAt from to horizon
  | from > to = []
  | otherwise = [Due p p p p d a | (i, d, a) <- zip3 [0 :: Int ..] dates amounts, let p = if i == 0 then from else d, d <= horizon]
  where
    k = max 1 n
    date = maybe from (max from) dueOn
    dates = [addGregorianMonthsClip (toInteger i) date | i <- [0 .. k - 1]]
    total = amountAt from
    rate = total `quot` toInteger k
    amounts = (total - rate * toInteger (k - 1)) : replicate (k - 1) rate
