{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Calendar
import System.Exit (exitFailure)

import Billing.Data
import Billing.Plan
import Billing.Schedule

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check name want got = unless (want == got) $ do
        putStrLn ("FEHLER " <> name <> "\n  erwartet: " <> show want <> "\n  bekommen: " <> show got)
        modifyIORef' failures (+ 1)
      d = fromGregorian
      dates = map dueDate
      amounts = map dueAmount
      fixed a = const a

  -- Monatlich vorschüssig am 3., volle Monate, bis Ende November
  let m = dues Monthly InAdvance 3 (fixed 100000) (d 2026 10 1) (d 2026 12 31) (d 2026 11 30)
  check "monatlich: Fälligkeiten" [d 2026 10 3, d 2026 11 3] (dates m)
  check "monatlich: Beträge" [100000, 100000] (amounts m)

  -- Beginn am 15.: Oktober anteilig 17/31, fällig nicht vor Beginn
  let s = dues Monthly InAdvance 1 (fixed 100000) (d 2026 10 15) (d 9999 12 31) (d 2026 11 1)
  check "Beginn 15.: Beträge" [54839, 100000] (amounts s)
  check "Beginn 15.: Fälligkeiten" [d 2026 10 15, d 2026 11 1] (dates s)
  check "Beginn 15.: abgedeckt" [(d 2026 10 15, d 2026 10 31)] [(dueFrom x, dueTo x) | x <- take 1 s]

  -- Mieterhöhung zum 15.10.: zwei Zeitscheiben teilen sich den Oktober
  let a1 = dues Monthly InAdvance 1 (fixed 100000) (d 2026 1 1) (d 2026 10 14) (d 2026 10 31)
      a2 = dues Monthly InAdvance 1 (fixed 120000) (d 2026 10 15) (d 9999 12 31) (d 2026 10 31)
  check "Anpassung: alter Satz im Oktober" 45161 (dueAmount (last a1))
  check "Anpassung: neuer Satz im Oktober" [65806] (amounts a2)
  check "Anpassung: Monate bis Oktober" 10 (length a1)

  -- Kündigung zum 20.11.: November anteilig, danach nichts mehr
  let k = dues Monthly InAdvance 1 (fixed 90000) (d 2026 11 1) (d 2026 11 20) (d 2027 12 31)
  check "Kündigung: anteilig 20/30" [60000] (amounts k)

  -- Quartal nachschüssig am 15. des Folgemonats
  let q = dues Quarterly InArrears 15 (fixed 30000) (d 2026 1 1) (d 2026 12 31) (d 2026 7 20)
  check "Quartal nachschüssig" [d 2026 4 15, d 2026 7 15] (dates q)
  check "Quartal: Perioden" [(d 2026 1 1, d 2026 3 31), (d 2026 4 1, d 2026 6 30)] [(duePeriodFrom x, duePeriodTo x) | x <- q]

  -- Fälligkeitstag 31 im Februar
  let f = dues Monthly InAdvance 31 (fixed 1) (d 2026 2 1) (d 2026 2 28) (d 2026 12 31)
  check "Tag 31 im Februar" [d 2026 2 28] (dates f)

  -- Jährlich, Halbjahr
  check "jährlich" [d 2026 1 1, d 2027 1 1] (dates (dues Yearly InAdvance 1 (fixed 5) (d 2026 1 1) (d 2027 12 31) (d 2027 6 1)))
  check "Halbjahr" [(d 2026 7 1, d 2026 12 31)] [periodOf HalfYearly (d 2026 9 9)]

  -- Einmalig: am Beginn, voller Betrag
  check "einmalig" [Due (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) 777] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 6 1))
  check "einmalig, noch nicht fällig" [] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 5 4))

  -- Runden
  check "roundDiv positiv (halbe Einheit aufrunden)" [2, 1, 2, 2] [roundDiv 3 2, roundDiv 4 3, roundDiv 5 3, roundDiv 7 4]
  check "roundDiv negativ" (-2) (roundDiv (-3) 2)

  planTests check

  n <- readIORef failures
  if n == 0 then putStrLn "alle Tests bestanden" else exitFailure

-- | Belegplanung: Richtung, Kontierung je Feldstatus, Meldungen, schon Gebuchtes.
planTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
planTests check = do
  let d = fromGregorian
      types =
        Map.fromList
          [ ("MV", ContractType "MV" "RECEIVABLE" (Just "1200") Nothing)
          , ("VS", ContractType "VS" "PAYABLE" (Just "2900") (Just "KR"))
          , ("XX", ContractType "XX" "RECEIVABLE" Nothing Nothing)
          ]
      contract' cid typ = Contract "1000" cid typ "ACTIVE" "P1" "EUR" "" (d 2026 1 1) openEnd
      cond cid typ acc amount =
        Condition cid typ "" False amount Nothing Monthly 3 InAdvance (Just acc) Nothing (d 2026 1 1) openEnd
      -- Feldstatus wie die Vorschläge des Ledgers (nicht genannt = ausgeblendet)
      status =
        Map.fromList
          [ (("CUSTOMER", f), "OPTIONAL") | f <- ["sd_customer_id", "rent_contract_id", "rent_object_id"] ]
          <> Map.fromList [(("RENT_REVENUE", f), "REQUIRED") | f <- ["rent_object_id", "rent_contract_id"]]
          <> Map.fromList [(("SUPPLIER", "supplier_id"), "REQUIRED"), (("SUPPLIER", "rent_object_id"), "OPTIONAL")]
          <> Map.fromList [(("BALANCE", f), "OPTIONAL") | f <- ["rent_object_id", "rent_contract_id"]]
      accounts =
        Map.fromList
          [ (a, Account a g False)
          | (a, g) <- [("1200", "CUSTOMER"), ("6000", "RENT_REVENUE"), ("2900", "SUPPLIER"), ("7300", "BALANCE")]
          ]
      input =
        Input
          { inTypes = types
          , inContracts = [contract' "MV-1" "MV", contract' "VS-1" "VS", contract' "XX-1" "XX"]
          , inConditions = [cond "MV-1" "KM" "SKR25-6000" 80000, cond "MV-1" "MIN" "6000" (-5000), cond "VS-1" "EN" "7300" 12000, cond "XX-1" "KM" "6000" 1]
          , inObjects = [ContractObject "MV-1" "RentObject" "WG001" True (d 2026 1 1) openEnd]
          , inMeasurements = []
          , inAccounts = accounts
          , inFieldStatus = status
          , inPosted = Set.fromList [("MV-1", "KM", "", d 2026 1 1)]
          }
      (docs, msgs) = plan (d 2026 2 28) input
      docOf cid date = [x | x <- docs, cId (dContract x) == cid, dDate x == date]
  check "Meldung ohne Abstimmkonto" [("XX-1", "Vertragsart XX: Abstimmkonto fehlt (Verträge → Vertragsarten)")] msgs
  -- Januar KM schon gebucht: nur die Minderung im Januar, im Februar beides
  check "schon Gebuchtes übersprungen" [["MIN"]] [map (kType . eCondition) (dEntries x) | x <- docOf "MV-1" (d 2026 1 3)]
  case docOf "MV-1" (d 2026 2 3) of
    [x] -> do
      let km = take 2 (dLines x)
          minus = drop 2 (dLines x)
      check "Forderung: Soll Debitor, Haben Erlös" [("1200", "S", 80000), ("SKR25-6000", "H", 80000)] [(lAccount l, lSide l, lAmount l) | l <- km]
      check "Debitor: Kunde, Vertrag, Objekt" [("rent_contract_id", "MV-1"), ("rent_object_id", "WG001"), ("sd_customer_id", "P1")] (sortFields (head' km))
      check "Erlös: ohne Kunde (ausgeblendet)" [("rent_contract_id", "MV-1"), ("rent_object_id", "WG001")] (sortFields (km !! 1))
      check "Minderung: Seiten getauscht" [("1200", "H", 5000), ("6000", "S", 5000)] [(lAccount l, lSide l, lAmount l) | l <- minus]
      check "Belegart Standard DR" "DR" (dDocumentType x)
    other -> check "ein Beleg im Februar" 1 (length other)
  case docOf "VS-1" (d 2026 1 3) of
    [x] -> do
      check "Verbindlichkeit: Soll Aufwand, Haben Kreditor" [("7300", "S"), ("2900", "H")] [(lAccount l, lSide l) | l <- dLines x]
      check "Kreditor: Lieferant und Objekt erlaubt, Vertrag ausgeblendet" [("supplier_id", "P1")] (sortFields (dLines x !! 1))
      check "Belegart der Vertragsart" "KR" (dDocumentType x)
    other -> check "ein Versicherungsbeleg im Januar" 1 (length other)
  where
    sortFields = Map.toList . Map.fromList . lFields
    head' = \case
      l : _ -> l
      [] -> Line "" "" 0 "" []
