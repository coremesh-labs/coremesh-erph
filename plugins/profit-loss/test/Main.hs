module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Data.Time.Calendar
import System.Exit (exitFailure)

import ProfitLoss.Api (parseSettings)
import ProfitLoss.Ledger
import ProfitLoss.Objects
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
        [ Item "4000" (d 2026 1 1) (-100000) "A" -- Miete Januar (Haben)
        , Item "4000" (d 2026 2 1) (-100000) "B"
        , Item "4000" (d 2025 12 1) (-100000) "A" -- vor dem Zeitraum
        , Item "6300" (d 2026 1 15) 30050 "A"
        , Item "6400" (d 2026 2 10) 12000 "B"
        , Item "6400" (d 2026 2 11) (-12000) "B" -- Storno: Konto ohne Bewegung entfällt
        , Item "2700" (d 2026 1 31) (-150) ""
        , Item "1200" (d 2026 1 1) 200000 "A" -- Bilanz: nicht in der GuV
        , Item "9999" (d 2026 1 1) 5 "A" -- unbekanntes Konto
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

  -- Spalten je Gruppe (Gebäude): Beträge je Gruppe, leer ohne Bewegung, zuletzt die Summe
  let byGroup = [(g, computeReport defaultSettings accounts (d 2026 1 1) (d 2026 2 28) [i | i <- items, itGroup i == g]) | g <- ["A", "B", ""]]
  check "Spalten je Gruppe" ["Bereich", "Konto", "Bezeichnung", "A", "B", "ohne", "Summe"] (tableColumnsBy ["A", "B", "ohne"])
  check "Zeilen je Gruppe"
    [ ["Erlöse", "", "", "", "", "", ""]
    , ["", "4000", "Mieterlöse", "1.000,00", "1.000,00", "", "2.000,00"]
    , ["Summe Erlöse", "", "", "1.000,00", "1.000,00", "", "2.000,00"]
    , ["Aufwendungen", "", "", "", "", "", ""]
    , ["", "6300", "Instandhaltung", "300,50", "", "", "300,50"]
    , ["Summe Aufwendungen", "", "", "300,50", "", "", "300,50"]
    , ["Neutrales Ergebnis", "", "", "", "", "", ""]
    , ["", "2700", "Zinserträge", "", "", "1,50", "1,50"]
    , ["Summe Neutrales Ergebnis", "", "", "", "", "1,50", "1,50"]
    , ["Ergebnis (Gewinn + / Verlust −)", "", "", "699,50", "1.000,00", "1,50", "1.701,00"]
    ]
    (tableRowsBy 2 rep byGroup)

  -- Gebäude eines Postens: Spalte, Mietobjekt, Vertrag (Hauptobjekt, gültig am Buchungsdatum)
  let objs =
        Objects
          { obBuildings = Map.fromList [("G1", ("Gartenstr. 1", "WE1")), ("G2", ("Hof 2", "WE1"))]
          , obRentObjects = Map.fromList [("W1", "G1"), ("W2", "G2")]
          , obContracts =
              Map.fromList
                [ ("MV1", [ContractObject "RentObject" "W2" True Nothing Nothing])
                , ("DA1", [ContractObject "Building" "G2" False Nothing Nothing, ContractObject "Building" "G1" True (Just (d 2026 3 1)) Nothing])
                , ("WE", [ContractObject "BusinessEntity" "WE1" True Nothing Nothing])
                ]
          , obEntities = Map.fromList [("WE1", "Wohnanlage")]
          }
  check "Gebäude aus der Spalte" "G1" (buildingOf objs "G1" "W2" "MV1" (d 2026 1 1))
  check "Mietobjekt in der Spalte" "G2" (buildingOf objs "W2" "" "" (d 2026 1 1))
  check "Gebäude über Mietobjekt" "G1" (buildingOf objs "" "W1" "MV1" (d 2026 1 1))
  check "Gebäude über Vertrag/Mietobjekt" "G2" (buildingOf objs "" "" "MV1" (d 2026 1 1))
  check "Vertrag: Hauptobjekt noch nicht gültig" "G2" (buildingOf objs "" "" "DA1" (d 2026 2 1))
  check "Vertrag: Hauptobjekt zuerst" "G1" (buildingOf objs "" "" "DA1" (d 2026 3 1))
  check "ohne Zuordnung" "" (buildingOf objs "" "" "X" (d 2026 1 1))
  check "Wirtschaftseinheit über Gebäude" "WE1" (entityOfPosting objs "G2" "" (d 2026 1 1))
  check "Wirtschaftseinheit über Vertrag" "WE1" (entityOfPosting objs "" "WE" (d 2026 1 1))
  check "Name Gebäude" "Gartenstr. 1 (G1)" (buildingName objs "G1")
  check "Name ohne Gebäude" "nicht zugeordnet" (buildingName objs "")

  -- Saldenliste und Kontoblatt: Vortrag vor „von“, laufender Saldo, ausgeglichen
  let po acc dt doc amt = Posting acc dt doc "" "" "" "" amt
      ps =
        [ po "1200" (d 2026 1 1) "1" 50000
        , po "1200" (d 2026 1 3) "2" (-50000)
        , po "1200" (d 2026 2 1) "3" 41500
        , po "3550" (d 2026 3 12) "4" (-96020)
        , po "3550" (d 2026 3 13) "5" 96020
        , po "1200" (d 2026 3 1) "6" 1 -- nach „bis“
        ]
      tb = trialBalance accounts (d 2026 2 1) (d 2026 2 28) [p | p <- ps, poAccount p == "1200"]
  check "Saldenliste" [BalanceRow (Account "1200" "Bank" "BALANCE_SHEET") 0 41500 0 41500] tb
  check "Saldenliste: nur offene" 1 (length (balanceRows 2 True (trialBalance accounts (d 2026 1 1) (d 2026 3 31) ps)) - 1)
  let sh = accountSheet (d 2026 1 2) (d 2026 2 28) [p | p <- ps, poAccount p == "1200"]
  check "Kontoblatt: Vortrag" 50000 (shOpening sh)
  check "Kontoblatt: laufender Saldo" [0, 41500] (map snd (shPostings sh))
  check "Kontoblatt: Schluss" (41500, 41500, 50000) (shClosing sh, shDebit sh, shCredit sh)
  check "ausgeglichen" "ausgeglichen" (balanceText (shClosing (accountSheet (d 2026 1 1) (d 2026 12 31) [p | p <- ps, poAccount p == "3550"])))
  check "Kontoblatt: Zeilen"
    [ ["02.01.2026", "", "Saldovortrag", "", "", "", "", "", "500,00"]
    , ["03.01.2026", "2", "", "", "", "", "", "500,00", "0,00"]
    , ["01.02.2026", "3", "", "", "", "", "415,00", "", "415,00"]
    , ["28.02.2026", "", "Summe / Saldo (Soll-Saldo)", "", "", "", "415,00", "500,00", "415,00"]
    ]
    (sheetRows 2 (d 2026 1 2) (d 2026 2 28) sh)

  -- Partnerkonto: Stand je Vertrag, ohne Vertrag zuletzt
  let pc c dt amt = (po "1200" dt "" amt) {poContract = c}
      cs = [pc "MV2" (d 2026 1 1) 500, pc "MV2" (d 2026 1 5) (-500), pc "" (d 2026 1 6) (-30), pc "MV1" (d 2026 1 1) 700, pc "MV1" (d 2026 9 1) 1]
  check "Stand je Vertrag" [ContractTotal "MV1" 700 0 700, ContractTotal "MV2" 500 500 0, ContractTotal "" 0 30 (-30)] (contractTotals (d 2026 6 30) cs)

  -- Debitoren und Kreditoren: Saldo je Partner und Konto, Summen je Konto, Art und gesamt
  let pp k acc bp dt amt = (k, (po acc dt "" amt) {poPartner = bp})
      pr = partnerBalances (d 2026 2 1) (d 2026 12 31)
        [ pp "D" "1200" "BP1" (d 2026 1 5) 1000
        , pp "D" "1200" "BP1" (d 2026 2 5) (-1000)
        , pp "D" "1200" "BP2" (d 2026 3 1) 500
        , pp "K" "3300" "BP3" (d 2026 3 1) (-700)
        ]
  check "Partnersalden" [PartnerRow "D" "1200" "BP1" 1000 0 1000 0, PartnerRow "D" "1200" "BP2" 0 500 0 500, PartnerRow "K" "3300" "BP3" 0 0 700 (-700)] pr
  let pl = partnerListRows 2 False (const "") (const "") pr
  check "Summe Konto 1200" ["1200", "", "Summe Konto 1200 (2 Partner)", "", "10,00", "5,00", "10,00", "5,00", "Soll-Saldo"] (pl !! 2)
  check "Summe Debitoren" ["", "", "Summe Debitoren (2 Partner)", "", "10,00", "5,00", "10,00", "5,00", "Soll-Saldo"] (pl !! 3)
  check "Gesamtsumme" ["", "", "Gesamtsumme (3 Partner)", "", "10,00", "5,00", "17,00", "-2,00", "Haben-Saldo"] (last pl)
  check "nur offene" 7 (length (partnerListRows 2 True (const "") (const "") pr))

  n <- readIORef failures
  if n > 0 then putStrLn (show n <> " Fehler") >> exitFailure else putStrLn "alle Tests bestanden"
