-- | GuV-Rechnung ohne Ein-/Ausgabe: Einzelposten → Bereiche, Summen, Ergebnis,
-- Tabelle und CSV.
--
-- Beträge in kleinster Einheit, Soll positiv, Haben negativ (wie im Hauptbuch).
-- Ein Bereich der Seite INCOME (Ertrag) zeigt Haben positiv, EXPENSE (Aufwand)
-- Soll positiv; Ergebnis = Erträge − Aufwendungen (Gewinn positiv). Welche
-- Kontotypen in welchen Bereich gehören, steht in den Einstellungen (settings.sections).
module ProfitLoss.Report
  ( Settings (..)
  , Section (..)
  , Side (..)
  , defaultSettings
  , Account (..)
  , Item (..)
  , Report (..)
  , SectionResult (..)
  , computeReport
  , tableRows
  , tableColumns
  , tableColumnsBy
  , tableRowsBy
  , csv
  , formatAmountDe
  , formatDay
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, toGregorian)

data Side = Income | Expense
  deriving stock (Eq, Show)

-- | Bereich der GuV: Titel, Seite, Kontotypen des Hauptbuchs (GLAccount.account_type).
data Section = Section
  { secTitle :: Text
  , secSide :: Side
  , secTypes :: [Text]
  }
  deriving stock (Eq, Show)

data Settings = Settings
  { setLedger :: Text
  -- ^ Ledger der Einzelposten (führendes Ledger 0L)
  , setSections :: [Section]
  , setBuildingField :: Text
  -- ^ Kontierungsspalte der Einzelposten mit dem Gebäude (z. B. dimension_custom_1)
  }
  deriving stock (Eq, Show)

-- | Vorschlag; in der Konfiguration (settings.sections) änderbar.
defaultSettings :: Settings
defaultSettings =
  Settings
    { setLedger = "0L"
    , setSections =
        [ Section "Erlöse" Income ["REVENUE"]
        , Section "Aufwendungen" Expense ["PRIMARY_COST"]
        , Section "Sekundäre Kosten" Expense ["SECONDARY_COST"]
        , Section "Neutrales Ergebnis" Income ["NON_OPERATING"]
        ]
    , setBuildingField = "dimension_custom_1"
    }

-- | Sachkonto mit Bezeichnung und Kontotyp.
data Account = Account
  { accNumber :: Text
  , accName :: Text
  , accType :: Text
  }
  deriving stock (Eq, Show)

-- | Einzelposten: Konto, Buchungsdatum, Betrag (Hauswährung, kleinste Einheit),
-- Gruppe für die Spalten (Gebäude bzw. Wirtschaftseinheit, "" = nicht zugeordnet).
data Item = Item
  { itAccount :: Text
  , itDate :: Day
  , itAmount :: Integer
  , itGroup :: Text
  }
  deriving stock (Eq, Show)

data SectionResult = SectionResult
  { srSection :: Section
  , srAccounts :: [(Account, Integer)]
  -- ^ Konto und Betrag in der Sicht des Bereichs (Ertrag bzw. Aufwand positiv)
  , srTotal :: Integer
  }
  deriving stock (Eq, Show)

data Report = Report
  { rpSections :: [SectionResult]
  , rpResult :: Integer
  -- ^ Gewinn positiv, Verlust negativ
  , rpItems :: Int
  -- ^ berücksichtigte Einzelposten
  }
  deriving stock (Eq, Show)

-- | Einzelposten im Zeitraum [von, bis] auf Konten der Bereiche; Konten ohne Bewegung entfallen.
computeReport :: Settings -> [Account] -> Day -> Day -> [Item] -> Report
computeReport Settings {..} accounts from to items =
  Report {rpSections = sections, rpResult = sum (map signedTotal sections), rpItems = length used}
  where
    byNumber = Map.fromList [(accNumber a, a) | a <- accounts]
    sectionOf a = lookup True [(accType a `elem` secTypes s, s) | s <- setSections]
    used =
      [ (s, a, itAmount i)
      | i <- items
      , itDate i >= from && itDate i <= to
      , Just a <- [Map.lookup (itAccount i) byNumber]
      , Just s <- [sectionOf a]
      ]
    sums = Map.fromListWith (+) [(accNumber a, v) | (_, a, v) <- used]
    sections =
      [ SectionResult s rows (sum (map snd rows))
      | s <- setSections
      , let rows =
              sortOn (accNumber . fst)
                [ (a, view s v)
                | (n, v) <- Map.toList sums
                , v /= 0
                , Just a <- [Map.lookup n byNumber]
                , sectionOf a == Just s
                ]
      , not (null rows)
      ]
    view s v = if secSide s == Income then negate v else v
    signedTotal r = if secSide (srSection r) == Income then srTotal r else negate (srTotal r)

tableColumns :: [Text]
tableColumns = ["Bereich", "Konto", "Bezeichnung", "Betrag"]

-- | Zeilen der Tabelle: je Bereich Kopf, Konten, Summe; zum Schluss das Ergebnis.
tableRows :: Int -> Report -> [[Text]]
tableRows dec Report {..} =
  concatMap section rpSections
    <> [["Ergebnis (Gewinn + / Verlust −)", "", "", formatAmountDe dec rpResult]]
  where
    section SectionResult {..} =
      [secTitle srSection, "", "", ""]
        : [["", accNumber a, accName a, formatAmountDe dec v] | (a, v) <- srAccounts]
        <> [["Summe " <> secTitle srSection, "", "", formatAmountDe dec srTotal]]

-- | Spalten der GuV mit einer Spalte je Gruppe (Bezeichnung) und der Summe.
tableColumnsBy :: [Text] -> [Text]
tableColumnsBy labels = ["Bereich", "Konto", "Bezeichnung"] <> labels <> ["Summe"]

-- | Zeilen der GuV je Gruppe: Gesamtbericht und je Gruppe (Schlüssel, Bericht);
-- Beträge in der Reihenfolge der Gruppen, zuletzt die Summe. Leer = keine Bewegung.
tableRowsBy :: Int -> Report -> [(Text, Report)] -> [[Text]]
tableRowsBy dec total groups =
  concatMap section (rpSections total)
    <> [["Ergebnis (Gewinn + / Verlust −)", "", ""] <> [amount (rpResult r) | (_, r) <- groups] <> [amount (rpResult total)]]
  where
    amount v = if v == 0 then "" else formatAmountDe dec v
    inGroup r s = lookup (secTitle (srSection s)) [(secTitle (srSection g), g) | g <- rpSections r]
    section s =
      ([secTitle (srSection s), "", ""] <> map (const "") groups <> [""])
        : [ ["", accNumber a, accName a]
              <> [amount (maybe 0 (\g -> fromMaybe 0 (lookup (accNumber a) [(accNumber b, v') | (b, v') <- srAccounts g])) (inGroup r s)) | (_, r) <- groups]
              <> [amount v]
          | (a, v) <- srAccounts s
          ]
        <> [["Summe " <> secTitle (srSection s), "", ""] <> [amount (maybe 0 srTotal (inGroup r s)) | (_, r) <- groups] <> [amount (srTotal s)]]

-- | CSV für Tabellenkalkulationen: Semikolon, Dezimalkomma, Kopfzeilen mit den Angaben der Maske.
csv :: [(Text, Text)] -> [Text] -> [[Text]] -> Text
csv header cols rows =
  T.unlines (map line ([[k, v] | (k, v) <- header] <> [[]] <> [cols] <> rows))
  where
    line = T.intercalate ";" . map field
    field t
      | T.any (`elem` [';', '"', '\n', '\r']) t = "\"" <> T.replace "\"" "\"\"" t <> "\""
      | otherwise = t

-- | Kleinste Einheit → deutscher Betrag (125050, 2 → "1.250,50").
formatAmountDe :: Int -> Integer -> Text
formatAmountDe decimals minor =
  let sign = if minor < 0 then "-" else ""
      s = show (abs minor)
      padded = replicate (decimals + 1 - length s) '0' <> s
      (whole, frac) = splitAt (length padded - decimals) padded
   in T.pack (sign <> groups whole <> (if decimals > 0 then "," <> frac else ""))
  where
    groups w = reverse (T.unpack (T.intercalate "." (T.chunksOf 3 (T.pack (reverse w)))))

formatDay :: Day -> Text
formatDay d = let (y, m, dd) = toGregorian d in T.pack (pad dd <> "." <> pad m <> "." <> show y)
  where
    pad n = (if n < 10 then "0" else "") <> show n
