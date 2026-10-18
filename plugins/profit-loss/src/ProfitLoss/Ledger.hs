-- | Summen- und Saldenliste und Kontoblatt ohne Ein-/Ausgabe.
--
-- Grundlage sind die Einzelposten eines Geschäftsjahres (fiscal_year der
-- Posten). Saldovortrag = Posten des Jahres vor „Zeitraum von“; Soll und
-- Haben = Posten im Zeitraum; Saldo = Vortrag + Soll − Haben (positiv =
-- Soll-Saldo, wie im Hauptbuch). Ein Konto ist ausgeglichen, wenn sein Saldo 0 ist.
module ProfitLoss.Ledger
  ( Posting (..)
  , BalanceRow (..)
  , trialBalance
  , balanceColumns
  , balanceRows
  , sheetColumns
  , sheetRows
  , Sheet (..)
  , accountSheet
  , balanceText
  , ContractTotal (..)
  , contractTotals
  , partnerRows
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)

import ProfitLoss.Report (Account (..), formatAmountDe, formatDay)

-- | Einzelposten mit allem, was Saldenliste und Kontoblatt zeigen.
data Posting = Posting
  { poAccount :: Text
  , poDate :: Day
  , poDocument :: Text
  , poText :: Text
  , poContract :: Text
  , poPartner :: Text
  , poBuilding :: Text
  -- ^ Bezeichnung des Gebäudes (schon aufgelöst)
  , poAmount :: Integer
  -- ^ Hauswährung, kleinste Einheit, Soll positiv
  }
  deriving stock (Eq, Show)

data BalanceRow = BalanceRow
  { brAccount :: Account
  , brOpening :: Integer
  , brDebit :: Integer
  , brCredit :: Integer
  -- ^ positiv
  , brClosing :: Integer
  }
  deriving stock (Eq, Show)

-- | Saldenliste: je Konto mit Posten bis „bis“ (unbekannte Konten mit ihrer Nummer).
trialBalance :: [Account] -> Day -> Day -> [Posting] -> [BalanceRow]
trialBalance accounts from to postings =
  [ BalanceRow acc o d c (o + d - c)
  | (n, (o, d, c)) <- Map.toAscList sums
  , let acc = Map.findWithDefault (Account n "" "") n byNumber
  ]
  where
    byNumber = Map.fromList [(accNumber a, a) | a <- accounts]
    sums =
      Map.fromListWith
        add
        [ (poAccount p, if poDate p < from then (poAmount p, 0, 0) else if poAmount p >= 0 then (0, poAmount p, 0) else (0, 0, negate (poAmount p)))
        | p <- postings
        , poDate p <= to
        ]
    add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

balanceColumns :: [Text]
balanceColumns = ["Konto", "Bezeichnung", "Saldovortrag", "Soll", "Haben", "Saldo", "Stand"]

-- | Zeilen der Saldenliste; nur offene = Konten mit Saldo ≠ 0. Zum Schluss die Summe.
balanceRows :: Int -> Bool -> [BalanceRow] -> [[Text]]
balanceRows dec onlyOpen rows =
  [ [accNumber brAccount, accName brAccount, amount brOpening, amount brDebit, amount brCredit, amount brClosing, balanceText brClosing]
  | BalanceRow {..} <- shown
  ]
    <> [["Summe", "", amount (sum (map brOpening shown)), amount (sum (map brDebit shown)), amount (sum (map brCredit shown)), amount (sum (map brClosing shown)), ""]]
  where
    shown = if onlyOpen then filter ((/= 0) . brClosing) rows else rows
    amount = formatAmountDe dec

-- | „ausgeglichen“, „Soll-Saldo“ bzw. „Haben-Saldo“.
balanceText :: Integer -> Text
balanceText v
  | v == 0 = "ausgeglichen"
  | v > 0 = "Soll-Saldo"
  | otherwise = "Haben-Saldo"

data Sheet = Sheet
  { shOpening :: Integer
  , shDebit :: Integer
  , shCredit :: Integer
  , shClosing :: Integer
  , shPostings :: [(Posting, Integer)]
  -- ^ Posten im Zeitraum mit laufendem Saldo
  }
  deriving stock (Eq, Show)

-- | Kontoblatt: Vortrag (vor „von“), Posten im Zeitraum nach Datum und Beleg mit laufendem Saldo.
accountSheet :: Day -> Day -> [Posting] -> Sheet
accountSheet from to postings =
  Sheet
    { shOpening = opening
    , shDebit = sum [poAmount p | p <- inPeriod, poAmount p > 0]
    , shCredit = negate (sum [poAmount p | p <- inPeriod, poAmount p < 0])
    , shClosing = opening + sum (map poAmount inPeriod)
    , shPostings = zip inPeriod (drop 1 (scanl (+) opening (map poAmount inPeriod)))
    }
  where
    opening = sum [poAmount p | p <- postings, poDate p < from]
    inPeriod = sortOn (\p -> (poDate p, poDocument p)) [p | p <- postings, poDate p >= from, poDate p <= to]

sheetColumns :: [Text]
sheetColumns = ["Datum", "Beleg", "Text", "Vertrag", "Partner", "Gebäude", "Soll", "Haben", "Saldo"]

sheetRows :: Int -> Day -> Day -> Sheet -> [[Text]]
sheetRows dec from to Sheet {..} =
  [formatDay from, "", "Saldovortrag", "", "", "", "", "", amount shOpening]
    : [ [formatDay (poDate p), poDocument p, poText p, poContract p, poPartner p, poBuilding p, debit (poAmount p), credit (poAmount p), amount s]
      | (p, s) <- shPostings
      ]
    <> [[formatDay to, "", "Summe / Saldo (" <> balanceText shClosing <> ")", "", "", "", amount shDebit, amount shCredit, amount shClosing]]
  where
    amount = formatAmountDe dec
    debit v = if v > 0 then amount v else ""
    credit v = if v < 0 then amount (negate v) else ""

-- | Summe der Partnerposten eines Vertrags (Vertrag "" = ohne Vertrag).
data ContractTotal = ContractTotal
  { ctContract :: Text
  , ctDebit :: Integer
  , ctCredit :: Integer
  , ctClosing :: Integer
  }
  deriving stock (Eq, Show)

-- | Je Vertrag Soll, Haben und Saldo (Posten bis „bis“), nach Vertrag sortiert, ohne Vertrag zuletzt.
contractTotals :: Day -> [Posting] -> [ContractTotal]
contractTotals to postings =
  sortOn (\c -> (ctContract c == "", ctContract c))
    [ ContractTotal c d k (d - k)
    | (c, (d, k)) <- Map.toList (Map.fromListWith add [(poContract p, split (poAmount p)) | p <- postings, poDate p <= to])
    ]
  where
    split v = if v >= 0 then (v, 0) else (0, negate v)
    add (a, b) (x, y) = (a + x, b + y)

-- | Partnerkonto: zuerst je Vertrag der Stand, dann alle Posten mit laufendem Saldo (Spalten wie das Kontoblatt).
partnerRows :: Int -> Day -> Day -> [ContractTotal] -> Sheet -> [[Text]]
partnerRows dec from to totals sh =
  [ ["", "", "Stand " <> (if T.null ctContract then "ohne Vertrag" else "Vertrag " <> ctContract) <> " (" <> balanceText ctClosing <> ")", ctContract, "", "", amount ctDebit, amount ctCredit, amount ctClosing]
  | ContractTotal {..} <- totals
  ]
    <> sheetRows dec from to sh
  where
    amount = formatAmountDe dec
