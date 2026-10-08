-- | Fälligkeiten einer Kondition: Perioden, anteilige Beträge, Fälligkeitstage.
-- Reine Funktionen ohne Ein-/Ausgabe.
--
--   * Perioden sind Kalenderperioden: Monat, Quartal (ab Jan/Apr/Jul/Okt),
--     Halbjahr (ab Jan/Jul), Jahr. Einmalig: ein Tag, der Beginn der Kondition.
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
  , periodOf
  , prorate
  , roundDiv
  ) where

import Data.Time.Calendar

data Frequency = Monthly | Quarterly | HalfYearly | Yearly | Once
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
periodOf f d =
  let (y, m, _) = toGregorian d
      months = case f of
        Monthly -> 1
        Quarterly -> 3
        HalfYearly -> 6
        Yearly -> 12
        Once -> 1
      startMonth = ((m - 1) `div` months) * months + 1
      start = fromGregorian y startMonth 1
      end = addDays (-1) (addGregorianMonthsClip (fromIntegral months) start)
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
dues Once _ _ amountAt from to horizon
  | from <= to && from <= horizon = [Due from from from from from (amountAt from)]
  | otherwise = []
dues f mode dueDay amountAt from to horizon = go (fst (periodOf f from))
  where
    go ps
      | ps > to = []
      | date > horizon = [] -- Fälligkeiten steigen mit der Periode
      | otherwise = Due a b ps pe date (prorate (amountAt a) (ps, pe) (a, b)) : go (addDays 1 pe)
      where
        pe = snd (periodOf f ps)
        a = max ps from
        b = min pe to
        date = case mode of
          InAdvance ->
            let (y, m, _) = toGregorian ps
             in max a (dayInMonth y m dueDay)
          InArrears ->
            let (y, m, _) = toGregorian (addDays 1 pe)
             in dayInMonth y m dueDay
