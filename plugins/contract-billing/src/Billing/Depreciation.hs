-- | Abschreibung (AfA) von Gebäuden und Wohnungen aus der Abschreibungskondition
-- eines Vertrags (contract: ContractDepreciation, z. B. Vertragsart KV
-- „Kaufvertrag Immobilie“).
--
-- Jährlich, monatsgenau: Im ersten Jahr zählen die Monate ab dem Monat des
-- Abschreibungsbeginns, im letzten bis zum Monat des Vertragsendes (Verkauf).
-- Linear: Bemessungsgrundlage × Satz; degressiv: Buchwert am Jahresanfang ×
-- Satz – jeweils × Monate / 12, höchstens bis zum Buchwert 0. Zeitscheiben:
-- Eine höhere Bemessungsgrundlage (nachträgliche Herstellungskosten) erhöht
-- den Buchwert um die Differenz und gilt für das ganze Jahr, in dem sie beginnt.
module Billing.Depreciation
  ( DepMethod (..)
  , DepTerms (..)
  , DepRow (..)
  , depSchedule
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)

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
                Linear -> fromIntegral (dpBase t) * dpRate t / 100 * fromIntegral months / 12
                Degressive -> fromIntegral before * dpRate t / 100 * fromIntegral months / 12
              amount = min before (round (raw :: Rational))
              row = DepRow yFrom yTo yTo months addition amount before (before - amount) t
           in if yTo < yFrom || yTo > horizon
                then []
                else row : (if before - amount <= 0 then [] else go (before - amount) (dpBase t) (y + 1))
     in go opening (dpBase first) y0

-- | Monate von a bis b (beide angebrochen voll gezählt).
monthsBetween :: Day -> Day -> Int
monthsBetween a b =
  let (ya, ma, _) = toGregorian a
      (yb, mb, _) = toGregorian b
   in fromIntegral (yb - ya) * 12 + (mb - ma) + 1
