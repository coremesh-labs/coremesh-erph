module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.Text (Text)

import Data.Time.Calendar
import System.Exit (exitFailure)

import ProfitLoss.Api (parseSettings)
import ProfitLoss.Report

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check name want got = unless (want == got) $ do
        putStrLn ("FEHLER " <> name <> "\n  erwartet: " <> show want <> "\n  bekommen: " <> show got)
        modifyIORef' failures (+ 1)
      d = fromGregorian
      accounts =
        [ Account "4000" "Mieterlöse" "REVENUE"
        , Account "6300" "Instandhaltung" "PRIMARY_COST"
        , Account "6400" "Versicherungen" "PRIMARY_COST"
        , Account "2700" "Zinserträge" "NON_OPERATING"
        , Account "1200" "Bank" "BALANCE_SHEET"
        ]
      items =
        [ Item "4000" (d 2026 1 1) (-100000) -- Miete Januar (Haben)
        , Item "4000" (d 2026 2 1) (-100000)
        , Item "4000" (d 2025 12 1) (-100000) -- vor dem Zeitraum
        , Item "6300" (d 2026 1 15) 30050
        , Item "6400" (d 2026 2 10) 12000
        , Item "6400" (d 2026 2 11) (-12000) -- Storno: Konto ohne Bewegung entfällt
        , Item "2700" (d 2026 1 31) (-150)
        , Item "1200" (d 2026 1 1) 200000 -- Bilanz: nicht in der GuV
        , Item "9999" (d 2026 1 1) 5 -- unbekanntes Konto
        ]
      rep = computeReport defaultSettings accounts (d 2026 1 1) (d 2026 2 28) items

  check "Bereiche" ["Erlöse", "Aufwendungen", "Neutrales Ergebnis"] (map (secTitle . srSection) (rpSections rep))
  check "Summen" [200000, 30050, 150] (map srTotal (rpSections rep))
  check "Ergebnis" (200000 - 30050 + 150) (rpResult rep)
  check "Einzelposten" 6 (rpItems rep)
  check "Tabelle" [ ["Erlöse", "", "", ""]
                  , ["", "4000", "Mieterlöse", "2.000,00"]
                  , ["Summe Erlöse", "", "", "2.000,00"]
                  , ["Aufwendungen", "", "", ""]
                  , ["", "6300", "Instandhaltung", "300,50"]
                  , ["Summe Aufwendungen", "", "", "300,50"]
                  , ["Neutrales Ergebnis", "", "", ""]
                  , ["", "2700", "Zinserträge", "1,50"]
                  , ["Summe Neutrales Ergebnis", "", "", "1,50"]
                  , ["Ergebnis (Gewinn + / Verlust −)", "", "", "1.701,00"]
                  ] (tableRows 2 rep)
  check "Verlust" "-1.234.567,89" (formatAmountDe 2 (-123456789))
  check "ohne Nachkommastellen" "1.000" (formatAmountDe 0 1000)
  check "kleine Beträge" "0,05" (formatAmountDe 2 5)
  check "CSV" "Buchungskreis;2000\n\nKonto;Betrag\n\"a;b\";\"1,5 \"\"x\"\"\"\n" (csv [("Buchungskreis", "2000")] ["Konto", "Betrag"] [["a;b", "1,5 \"x\""]])

  -- Einstellungen: eigene Bereiche, anderes Ledger; ungültig → Vorschlag
  let custom =
        parseSettings $
          KM.fromList
            [ ("ledger", String "2L")
            , ("sections", toJSON [object ["title" .= ("Erträge" :: Text), "side" .= ("INCOME" :: Text), "account_types" .= (["REVENUE", "NON_OPERATING"] :: [Text])]])
            ]
  check "Einstellungen: Ledger" "2L" (setLedger custom)
  check "Einstellungen: Bereiche" [Section "Erträge" Income ["REVENUE", "NON_OPERATING"]] (setSections custom)
  check "Einstellungen: Vorschlag" defaultSettings (parseSettings (KM.fromList [("sections", String "x")]))
  let only = computeReport custom accounts (d 2026 1 1) (d 2026 2 28) items
  check "eigene Bereiche: Ergebnis" 200150 (rpResult only)

  n <- readIORef failures
  if n > 0 then putStrLn (show n <> " Fehler") >> exitFailure else putStrLn "alle Tests bestanden"
