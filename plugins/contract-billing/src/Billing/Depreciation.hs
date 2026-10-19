-- | Abschreibung (AfA) von Gebäuden und Wohnungen aus der Abschreibungskondition
-- eines Vertrags (contract: ContractDepreciation, z. B. Vertragsart KV
-- „Kaufvertrag Immobilie“).
--
-- Jährlich, monatsgenau: Im ersten Jahr zählen die Monate ab dem Monat des
-- Abschreibungsbeginns, im letzten bis zum Monat des Vertragsendes (Verkauf).
-- Linear: Bemessungsgrundlage × Satz; degressiv: Buchwert am Jahresanfang ×
-- Satz (oder fester Jahresbetrag, z. B. laut Steuerbescheid) – jeweils × Monate
-- / 12, gerundet nach der Rundungsregel (z. B. auf volle Euro aufrunden),
-- höchstens bis zum Buchwert 0. „Raten im Jahr“ (depInstallments) teilt
-- den Jahresbetrag nach Monaten auf die Kalenderperioden (Monat, Quartal …);
-- der Rundungsrest kommt in die letzte Rate des Jahres. Zeitscheiben:
-- Eine höhere Bemessungsgrundlage (nachträgliche Herstellungskosten) erhöht
-- den Buchwert um die Differenz und gilt für das ganze Jahr, in dem sie beginnt.
module Billing.Depreciation
  ( DepMethod (..)
  , DepTerms (..)
  , DepRow (..)
  , depSchedule
  , depInstallments
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)

import Billing.Schedule (roundBy)

data DepMethod = Linear | Degressive
  deriving stock (Show, Eq)

data DepTerms = DepTerms
  { dpContract :: Text
  , dpFrom :: Day -- ^ Zeitscheibe gültig ab
  , dpTo :: Day
  , dpBase :: Integer -- ^ Bemessungsgrundlage Gebäude (kleinste Einheit), kumuliert
  , dpRate :: Rational -- ^ Prozent p. a.
  , dpMethod :: DepMethod
  , dpStart :: Day -- ^ Beginn der Abschreibung (erste Zeitscheibe maßgeblich)
  , dpOpening :: Maybe Integer -- ^ Restbuchwert zum Beginn (Übernahme), sonst = Bemessungsgrundlage
  , dpAssetAccount :: Text
  , dpExpenseAccount :: Text
  , dpType :: Text -- ^ Konditionsart des Vermerks (z. B. AF)
  , dpAnnual :: Maybe Integer -- ^ fester Jahresbetrag (linear), sonst Bemessungsgrundlage × Satz
  , dpInstallments :: Int -- ^ Raten im Jahr (Teiler von 12)
  , dpRoundingCode :: Maybe Text -- ^ Rundungsregel des Jahresbetrags (leer = Konditionsart)
  , dpRound :: Maybe (Text, Rational) -- ^ aufgelöst (Billing.Pricing.depRounding): Art, Schrittweite in der kleinsten Einheit
  }
  deriving stock (Show)

data DepRow = DepRow
  { drFrom :: Day
  , drTo :: Day
  , drDue :: Day -- ^ Jahresende bzw. Vertragsende
  , drMonths :: Int
  , drAddition :: Integer -- ^ Zugang im Jahr (nachträgliche Herstellungskosten)
  , drAmount :: Integer
  , drBookBefore :: Integer
  , drBookAfter :: Integer
  , drTerms :: DepTerms
  }
  deriving stock (Show)

-- | Plan bis zum Vertragsende bzw. horizon (Fälligkeit ≤ horizon).
depSchedule :: [DepTerms] -> Day -> Day -> [DepRow]
depSchedule terms end horizon = case sortOn dpFrom terms of
  [] -> []
  sorted@(first : _) ->
    let start = dpStart first
        (y0, _, _) = toGregorian start
        (yEnd, _, _) = toGregorian (min end horizon)
        opening = maybe (dpBase first) id (dpOpening first)
        go _ _ y | y > yEnd = []
        go book prevBase y =
          let yFrom = max start (fromGregorian y 1 1)
              yTo = min end (fromGregorian y 12 31)
              t = last (first : [s | s <- sorted, dpFrom s <= yTo])
              addition = max 0 (dpBase t - prevBase)
              before = book + addition
              months = monthsBetween yFrom yTo
              raw = case dpMethod t of
                Linear -> maybe (fromIntegral (dpBase t) * dpRate t / 100) fromIntegral (dpAnnual t) * fromIntegral months / 12
                Degressive -> fromIntegral before * dpRate t / 100 * fromIntegral months / 12
              amount = min before (roundYear t raw)
              row = DepRow yFrom yTo yTo months addition amount before (before - amount) t
           in if yTo < yFrom || yTo > horizon
                then []
                else row : (if before - amount <= 0 then [] else go (before - amount) (dpBase t) (y + 1))
     in go opening (dpBase first) y0

-- | Jahresbetrag (kleinste Einheit) nach der Rundungsregel, z. B. auf volle Euro
-- aufrunden; ohne Regel auf die kleinste Einheit.
roundYear :: DepTerms -> Rational -> Integer
roundYear t raw = case dpRound t of
  Just (mode, step) | step > 0 -> roundBy "HALF_UP" (fromInteger (roundBy mode (raw / step)) * step)
  _ -> round raw

-- | Raten einer Jahreszeile: je Kalenderperiode (12 / Raten Monate, ab Januar)
-- im Zeitraum der Zeile ein Anteil nach Monaten, gerundet; der Rundungsrest
-- kommt in die letzte Rate. Fällig zum Periodenende; der Zugang zählt in der ersten.
depInstallments :: DepRow -> [DepRow]
depInstallments row
  | n <= 1 || length periods <= 1 = [row]
  | otherwise = go (drBookBefore row) (zip3 periods amounts (True : repeat False))
  where
    n = dpInstallments (drTerms row)
    len = 12 `div` n
    (y, m, _) = toGregorian (drFrom row)
    firstStart = fromGregorian y (((m - 1) `div` len) * len + 1) 1
    periods =
      [ (max (drFrom row) ps, min (drTo row) pe)
      | ps <- takeWhile (<= drTo row) (iterate (addGregorianMonthsClip (fromIntegral len)) firstStart)
      , let pe = addDays (-1) (addGregorianMonthsClip (fromIntegral len) ps)
      ]
    share (a, b) = round (fromIntegral (drAmount row) * fromIntegral (monthsBetween a b) / fromIntegral (drMonths row) :: Rational)
    regular = map share (init periods)
    amounts = regular <> [drAmount row - sum regular]
    go _ [] = []
    go book (((a, b), amt, isFirst) : rest) =
      row {drFrom = a, drTo = b, drDue = b, drMonths = monthsBetween a b, drAddition = if isFirst then drAddition row else 0, drAmount = amt, drBookBefore = book, drBookAfter = book - amt}
        : go (book - amt) rest

-- | Monate von a bis b (beide angebrochen voll gezählt).
monthsBetween :: Day -> Day -> Int
monthsBetween a b =
  let (ya, ma, _) = toGregorian a
      (yb, mb, _) = toGregorian b
   in fromIntegral (yb - ya) * 12 + (mb - ma) + 1
