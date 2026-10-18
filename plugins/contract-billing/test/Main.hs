{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.List (nub, sort)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Time.Calendar
import System.Exit (exitFailure)

import Billing.Data
import Billing.Depreciation
import Billing.Loan
import Billing.OpCost
import Billing.Plan
import Billing.Pricing
import Billing.Schedule
import Billing.Settlement

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

  -- Rechnungsvorlage aus Abrechnungsweg der Vertragsart und Konditionsart (Rohdaten ohne is_template)
  let recs cols = records cols . map (map String)
      types = recs ["code", "billing_mode"] [["MV", "POSTING"], ["DLHW", "INVOICE"], ["VE", "BOTH"]]
      kinds = recs ["code", "billing_form"] [["FO", "CLAIM"], ["RP", "INVOICE"]]
      cond t k = head (recs ["contract_type", "condition_kind"] [[t, k]])
  check "Vorlage: Sollstellung" False (isTemplate types kinds (cond "MV" "FO"))
  check "Vorlage: Eingangsrechnung" True (isTemplate types kinds (cond "DLHW" "FO"))
  check "Vorlage: beides, Forderung" False (isTemplate types kinds (cond "VE" "FO"))
  check "Vorlage: beides, Rechnungsposition" True (isTemplate types kinds (cond "VE" "RP"))

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

  -- Vertrag ab dem 1., Zahlung am 15. (Arbeitgeber zahlt zur Monatsmitte)
  let p15 = dues Monthly InAdvance 15 (fixed 100000) (d 2026 10 1) (d 9999 12 31) (d 2026 12 31)
  check "am 15.: Fälligkeiten" [d 2026 10 15, d 2026 11 15, d 2026 12 15] (dates p15)
  check "am 15.: volle Monate" [100000, 100000, 100000] (amounts p15)
  check "am 15.: noch nicht fällig am 14." 1 (length (dues Monthly InAdvance 15 (fixed 1) (d 2026 10 1) (d 9999 12 31) (d 2026 11 14)))
  -- Beginn am 20., Zahlung am 15.: erster Monat anteilig am Beginn, dann am 15.
  let p20 = dues Monthly InAdvance 15 (fixed 93000) (d 2026 10 20) (d 9999 12 31) (d 2026 11 30)
  check "Beginn 20., am 15.: Fälligkeiten" [d 2026 10 20, d 2026 11 15] (dates p20)
  check "Beginn 20., am 15.: Oktober 12/31" [36000, 93000] (amounts p20)

  -- Fälligkeitstag 31 im Februar
  let f = dues Monthly InAdvance 31 (fixed 1) (d 2026 2 1) (d 2026 2 28) (d 2026 12 31)
  check "Tag 31 im Februar" [d 2026 2 28] (dates f)

  -- Jährlich, Halbjahr
  check "jährlich" [d 2026 1 1, d 2027 1 1] (dates (dues Yearly InAdvance 1 (fixed 5) (d 2026 1 1) (d 2027 12 31) (d 2027 6 1)))
  check "Halbjahr" [(d 2026 7 1, d 2026 12 31)] [periodOf HalfYearly (d 2026 9 9)]
  check "zweimonatlich" [(d 2026 3 1, d 2026 4 30), (d 2026 1 1, d 2026 2 28)] [periodOf Bimonthly (d 2026 3 17), periodOf Bimonthly (d 2026 2 2)]
  check "Versicherungsjahr Okt–Sep" [(d 2026 10 1, d 2027 9 30), (d 2025 10 1, d 2026 9 30)] [periodFrom 10 Yearly (d 2026 10 1), periodFrom 10 Yearly (d 2026 9 21)]
  check "Quartal ab Feb" [(d 2026 2 1, d 2026 4 30), (d 2025 11 1, d 2026 1 31)] [periodFrom 2 Quarterly (d 2026 3 5), periodFrom 2 Quarterly (d 2026 1 9)]
  check "Jahresmiete Jan–Dez, fällig 15.03." [(d 2026 3 15, d 2026 1 1, d 2026 12 31)] [(dueDate x, duePeriodFrom x, duePeriodTo x) | x <- duesFrom 1 3 Yearly InAdvance 15 (const 53621) (d 2026 1 1) openEnd (d 2026 12 31)]
  check "jährlich ab Okt: fällig 01.10." [(d 2025 10 1, 8925), (d 2026 10 1, 8925)] [(dueDate x, dueAmount x) | x <- duesFrom 10 1 Yearly InAdvance 1 (const 8925) (d 2025 10 1) openEnd (d 2026 12 31)]

  -- Einmalig: am Beginn, voller Betrag
  check "einmalig" [Due (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) 777] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 6 1))
  check "einmalig, noch nicht fällig" [] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 5 4))
  -- Einmalig mit eigenem Fälligkeitsdatum (Bereitstellungsgebühr bei Übergabe)
  check "einmalig am Fälligkeitsdatum" [d 2026 6 1] (dates (dueOnce 1 (Just (d 2026 6 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 6 30)))
  check "einmalig: Fälligkeit nach dem Stichtag" [] (dueOnce 1 (Just (d 2026 6 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31))
  check "einmalig: Fälligkeit vor Beginn gilt ab Beginn" [d 2026 5 5] (dates (dueOnce 1 (Just (d 2026 4 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31)))
  -- Kaution in drei Monatsraten: Rest auf die erste Rate, je Rate eine Fälligkeit
  let dep = dueOnce 3 Nothing (fixed 255001) (d 2026 1 31) openEnd (d 2026 12 31)
  check "Kaution: Raten" [85001, 85000, 85000] (amounts dep)
  check "Kaution: Fälligkeiten (Monatsende)" [d 2026 1 31, d 2026 2 28, d 2026 3 31] (dates dep)
  check "Kaution: erste Rate deckt den Beginn ab" [d 2026 1 31, d 2026 2 28, d 2026 3 31] (map dueFrom dep)
  check "einmalig mit Fälligkeit: Periode = Beginn" [(d 2026 5 5, d 2026 6 1)] [(dueFrom x, dueDate x) | x <- dueOnce 1 (Just (d 2026 6 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 6 30)]
  check "Kaution: nur fällige Raten" 2 (length (dueOnce 3 Nothing (fixed 3) (d 2026 1 31) openEnd (d 2026 3 30)))
  check "Rückzahlung (negativ) in Raten" [-34, -33, -33] (amounts (dueOnce 3 Nothing (fixed (-100)) (d 2026 1 1) openEnd (d 2026 12 31)))
  check "einmalig ohne Datum: am Beginn" [d 2026 5 5] (dates (dueOnce 1 Nothing (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31)))

  -- Runden
  check "roundDiv positiv (halbe Einheit aufrunden)" [2, 1, 2, 2] [roundDiv 3 2, roundDiv 4 3, roundDiv 5 3, roundDiv 7 4]
  check "roundDiv negativ" (-2) (roundDiv (-3) 2)

  planTests check
  loanTests check
  depreciationTests check
  settlementTests check
  opCostTests check

  n <- readIORef failures
  if n == 0 then putStrLn "alle Tests bestanden" else exitFailure

-- | Belegplanung: Richtung, Kontierung je Feldstatus, Meldungen, schon Gebuchtes.
planTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
planTests check = do
  let d = fromGregorian
      types =
        Map.fromList
          [ ("MV", ContractType "MV" "RECEIVABLE" "TENANT" Nothing Nothing False)
          , ("VS", ContractType "VS" "PAYABLE" "CREDITOR" (Just "KR") (Just "KX") True)
          , ("XX", ContractType "XX" "RECEIVABLE" "GUARANTOR" Nothing Nothing False)
          ]
      contract' cid typ = Contract "1000" cid typ "ACTIVE" "P1" "EUR" "" (d 2026 1 1) openEnd Nothing
      cond cid typ acc amount =
        Condition cid typ "" 1 False amount Nothing Monthly 3 1 1 Nothing 1 InAdvance (Just acc) Nothing (d 2026 1 1) openEnd Nothing Nothing Nothing
      -- Vermerk (vorerfasst oder gebucht) einer ganzen Monatsperiode
      booked cid typ acc y m amount =
        Posted cid typ "" 1 (d y m 1) (d y m (gregorianMonthLength y m)) (Just (d y m 1)) (d y m 3) amount Original acc (Just "WG001")
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
          , inConditions = [cond "MV-1" "KM" "6000" 80000, cond "MV-1" "MIN" "6000" (-5000), cond "VS-1" "EN" "7300" 12000, cond "XX-1" "KM" "6000" 1]
          , inObjects = [ContractObject "MV-1" "RentObject" "WG001" True (d 2026 1 1) openEnd]
          , inMeasurements = []
          , inAccounts = accounts
          , inFieldStatus = status
          , inLoans = []
          , inLoanPayments = []
          , inDepreciations = []
          , inPartnerAccounts =
              Map.fromList
                [ (("P1", "TENANT"), PartnerAccount "P1" "TENANT" (Just "1200") False)
                , (("P1", "CREDITOR"), PartnerAccount "P1" "CREDITOR" (Just "2900") False)
                , (("JC", "TENANT"), PartnerAccount "JC" "TENANT" (Just "1210") False)
                , (("ZZ", "TENANT"), PartnerAccount "ZZ" "TENANT" (Just "1200") True)
                ]
          , inPosted = [booked "MV-1" "KM" "6000" 2026 1 80000]
          , inCorrectionDate = d 2026 2 20
          , inBackdateFrom = Nothing
          , inConditionTypes = Map.empty, inDependentTypes = Map.empty, inTaxCodes = Map.empty, inRoundings = Map.empty, inCompanyRounding = Nothing, inDecimals = Map.empty
          }
      (docs, msgs) = plan (d 2026 2 28) input
      docOf cid date = [x | x <- docs, cId (dContract x) == cid, dDate x == date]
  -- Sollstellung ab (Übernahme): VS-1 erst ab 01.02., Januar entfällt
  let (late, _) = plan (d 2026 2 28) input {inContracts = [(contract' "VS-1" "VS") {cBillingStart = Just (d 2026 2 1)}], inPosted = []}
  check "Sollstellung ab: nur Februar" [d 2026 2 3] [dDate x | x <- late]
  -- Versicherungsjahr Okt–Sep, Kondition seit Okt 2025, Sollstellung ab 01.01.2026:
  -- die im Altsystem fällige Prämie (01.10.2025) entfällt, keine anteilige Periode ab Januar
  let ins = (cond "VS-1" "EN" "7300" 8925) {kFrequency = Yearly, kDueDay = 1, kStartMonth = 10, kDueMonth = 1, kFrom = d 2025 10 1}
      (yr, _) = plan (d 2027 12 31) input {inContracts = [(contract' "VS-1" "VS") {cFrom = d 2014 10 1, cBillingStart = Just (d 2026 1 1)}],
                                           inConditions = [ins], inPosted = []}
  check "Versicherungsjahr: fällig 01.10., volle Prämie" [(d 2026 10 1, 8925), (d 2027 10 1, 8925)]
    [(dDate x, sum (map (dueAmount . eDue) (dEntries x))) | x <- yr]
  check "Meldung ohne Partnerkonto" [("XX-1", "Partnerkonto fehlt: P1 hat in der Rolle GUARANTOR keine Buchungskreisdaten (Partnermodul) – Vertrag nicht gebucht")] msgs
  -- Januar KM schon gebucht: die Minderung für Januar ist nachträglich (Nachberechnung am Lauftag), im Februar beides
  check "schon Gebuchtes übersprungen" 0 (length (docOf "MV-1" (d 2026 1 3)))
  check "Januar nachträglich" [[("MIN", Correction)]] [[(kType (eCondition e), eKind e) | e <- dEntries x] | x <- docOf "MV-1" (d 2026 2 20)]
  case docOf "MV-1" (d 2026 2 3) of
    [x] -> do
      let km = take 2 (dLines x)
          minus = drop 2 (dLines x)
      check "Forderung: Soll Debitor, Haben Erlös" [("1200", "S", 80000), ("6000", "H", 80000)] [(lAccount l, lSide l, lAmount l) | l <- km]
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
  -- Nachgelagert (seit 0.13.0): Materialanteil 10 % als Anteil der Kaltmiete (eigenes Konto),
  -- Umsatzsteuer 19 % als Zuschlag auf alles außer Steuer; je Einheit mit genauem Einzelpreis
  let kfm = Rounding "KFM" "HALF_UP" (1 / 100) True True
      depIn =
        input
          { inContracts = [contract' "MV-1" "MV"]
          , inConditions =
              [ cond "MV-1" "KM" "6000" 80005
              , (cond "MV-1" "MAT" "6000" 0) {kDependent = Just "MAT", kAccount = Just "7300"}
              , (cond "MV-1" "UST" "6000" 0) {kDependent = Just "UST", kAccount = Nothing}
              ]
          , inPosted = []
          , inConditionTypes = Map.fromList [("KM", CondType "KM" (Just "U19") Nothing)]
          , inDependentTypes =
              Map.fromList
                [ ("MAT", Dependent "MAT" "Material" "PERCENT" Nothing (Just 10) "SHARE" ["KM"] Nothing Nothing 10)
                , ("UST", Dependent "UST" "USt" "TAX" Nothing Nothing "SURCHARGE" [] Nothing Nothing 90)
                ]
          , inTaxCodes = Map.fromList [("U19", TaxCode "U19" 19 (Just "7300"))]
          , inRoundings = Map.fromList [("KFM", kfm)]
          , inDecimals = Map.fromList [("EUR", 2)]
          }
      (depDocs, depMsgs) = plan (d 2026 1 31) depIn
  check "nachgelagert: keine Meldung" [] depMsgs
  check "nachgelagert: KM ohne Anteil, Anteil, Steuer auf 800,05"
    [("KM", 72004), ("MAT", 8001), ("UST", 15201)]
    (sort [(kType (eCondition e), dueAmount (eDue e)) | x <- depDocs, e <- dEntries x])
  check "Rundung: Bank 2,345 → 2,34, kaufmännisch 2,35, Rappen 2,37 → 2,35"
    [234, 235, 235]
    [roundMinor (Rounding "" "HALF_EVEN" (1 / 100) False True) 2 2.345, roundMinor kfm 2 2.345, roundMinor (Rounding "" "HALF_UP" (5 / 100) False True) 2 2.37]
  -- Je Einheit ohne Objekt: Bemessung des Hauptobjekts (qm-Preis × Wohnfläche)
  let qm =
        input
          { inContracts = [contract' "MV-1" "MV"]
          , inConditions = [(cond "MV-1" "KM" "6000" 447) {kPerUnit = True, kMeasurement = Just "WFL", kUnitPrice = Just "4.4678"}]
          , inMeasurements = [Measurement "WG001" "WFL" 83.9 (d 2000 1 1) openEnd]
          , inPosted = []
          , inRoundings = Map.fromList [("KFM", kfm)]
          , inDecimals = Map.fromList [("EUR", 2)]
          }
      (qmDocs, qmMsgs) = plan (d 2026 1 31) qm
  check "je Einheit am Hauptobjekt: keine Meldung" [] qmMsgs
  check "je Einheit am Hauptobjekt: 4,4678 × 83,9 = 374,85" [37485] [dueAmount (eDue e) | x <- qmDocs, e <- dEntries x]
  check "Dezimaltext" [Just 1250.5, Just 19, Nothing] [parseDecimal "1.250,5", parseDecimal "19", parseDecimal "1/2"]
  -- Überschneidende Zeitscheiben derselben Kondition: Meldung, kein Beleg
  let twice = input {inConditions = [cond "MV-1" "KM" "6000" 80000, (cond "MV-1" "KM" "6000" 85000) {kFrom = d 2026 2 1}], inPosted = mempty}
      (docs2, msgs2) = plan (d 2026 2 28) twice
  check "Überschneidung: kein Beleg für den Vertrag" 0 (length [x | x <- docs2, cId (dContract x) == "MV-1"])
  check "Überschneidung: Meldung" [("MV-1", "Kondition KM: Zeitscheiben überschneiden sich (01.02.2026) – Vertrag nicht gebucht")] [m | m@("MV-1", _) <- msgs2]
  -- Derselbe Gegenstand zweimal (Position 2): keine Überschneidung, beide gebucht
  let pos2 = input {inConditions = [cond "MV-1" "KM" "6000" 80000, (cond "MV-1" "KM" "6000" 5000) {kPos = 2}], inPosted = mempty}
      (docs3, msgs3) = plan (d 2026 1 31) pos2
  check "Position 2: keine Meldung" [] [m | m@("MV-1", _) <- msgs3]
  check "Position 2: beide Beträge" [5000, 80000] (sort [dueAmount (eDue e) | x <- docs3, cId (dContract x) == "MV-1", e <- dEntries x])

  -- Abweichender Zahler: sein Abstimmkonto; ohne Konto oder gesperrt: Vertrag nicht gebucht
  let payerInput pay = input {inContracts = take 1 (inContracts input), inConditions = [(cond "MV-1" "KM" "6000" 80000) {kPayer = Just pay}], inPosted = []}
      (jcDocs, _) = plan (d 2026 1 31) (payerInput "JC")
  check "Zahler: sein Abstimmkonto und Kunde" [("1210", "JC")] [(lAccount l, v) | x <- jcDocs, l <- take 1 (dLines x), ("sd_customer_id", v) <- lFields l]
  check "Zahler ohne Konto" [("MV-1", "Partnerkonto fehlt: NN hat in der Rolle TENANT keine Buchungskreisdaten (Partnermodul) – Vertrag nicht gebucht")]
    (snd (plan (d 2026 1 31) (payerInput "NN")))
  check "Zahler gesperrt" ([], [("MV-1", "Buchungssperre: ZZ in der Rolle TENANT – Vertrag nicht gebucht")])
    (let (ds, ms) = plan (d 2026 1 31) (payerInput "ZZ") in (map dDate ds, ms))

  correctionTests check input booked cond
  where
    sortFields = Map.toList . Map.fromList . lFields
    head' = \case
      l : _ -> l
      [] -> Line "" "" 0 "" []

-- | Nachberechnung: Vermerktes gegen neu Gerechnetes je Kalenderperiode.
correctionTests
  :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ())
  -> Input
  -> (Text -> Text -> Text -> Integer -> Int -> Integer -> Posted)
  -> (Text -> Text -> Text -> Integer -> Condition)
  -> IO ()
correctionTests check input booked cond = do
  let d = fromGregorian
      mv = input {inContracts = take 1 (inContracts input)}
      -- Januar und Februar sind mit 800,00 gebucht.
      janFeb = [booked "MV-1" "KM" "6000" 2026 m 80000 | m <- [1, 2]]
      run inp = plan (d 2026 2 28) inp
      corr docs = [(dDate x, dDocumentType x, [(kType (eCondition e), duePeriodFrom (eDue e), dueAmount (eDue e)) | e <- dEntries x, eKind e == Correction]) | x <- docs]
      originals docs = [(kType (eCondition e), dueDate (eDue e)) | x <- docs, e <- dEntries x, eKind e == Original]

  -- Unverändert: nichts
  let (same, _) = run mv {inConditions = [cond "MV-1" "KM" "6000" 80000], inPosted = janFeb}
  check "unverändert: kein Beleg" 0 (length same)

  -- Rückwirkende Minderung der Kaltmiete auf 750,00 ab Januar: zwei Gutschriften am Lauftag
  let (lower, _) = run mv {inConditions = [cond "MV-1" "KM" "6000" 75000], inPosted = janFeb}
  check "Minderung: Gutschrift am Lauftag" [(d 2026 2 20, "DG", [("KM", d 2026 1 1, -5000), ("KM", d 2026 2 1, -5000)])] (corr lower)
  case lower of
    [x] -> do
      check "Minderung: Seiten getauscht" [("1200", "H", 5000), ("6000", "S", 5000)] [(lAccount l, lSide l, lAmount l) | l <- take 2 (dLines x)]
      check "Minderung: Text" ["Nachberechnung KM 1/2026"] (map lText (take 1 (dLines x)))
    _ -> pure ()

  -- Mieterhöhung zum 15.02. rückwirkend: Februar anteilig nachbelasten (Rechnung DR)
  let raise = [(cond "MV-1" "KM" "6000" 80000) {kTo = d 2026 2 14}, (cond "MV-1" "KM" "6000" 94000) {kFrom = d 2026 2 15}]
      (higher, _) = run mv {inConditions = raise, inPosted = janFeb}
  check "Erhöhung: Differenz Februar 14/28 × 140,00" [(d 2026 2 20, "DR", [("KM", d 2026 2 1, 7000)])] (corr higher)

  -- Minderung als eigene Konditionsart, im März für Januar/Februar erfasst: Nachberechnung, nicht zur alten Fälligkeit
  let (late, _) = run mv {inConditions = [cond "MV-1" "KM" "6000" 80000, (cond "MV-1" "MIN" "6000" (-10000)) {kTo = d 2026 2 28}], inPosted = janFeb}
  check "nachträglich erfasst: Nachberechnung" [(d 2026 2 20, "DG", [("MIN", d 2026 1 1, -10000), ("MIN", d 2026 2 1, -10000)])] (corr late)
  check "nachträglich erfasst: keine Sollstellung zur alten Fälligkeit" [] (originals late)

  -- Neue Kondition, die erst nach dem Lauftag fällig ist: zur Fälligkeit, auch wenn spätere Monate schon vorab gebucht sind
  let ahead = [booked "MV-1" "KM" "6000" 2026 m 80000 | m <- [1 .. 4]]
      fee = (cond "MV-1" "BG" "6000" 15000) {kFrequency = Once, kFrom = d 2026 2 20, kDueDate = Just (d 2026 3 15)}
      (feeDocs, _) = plan (d 2026 4 30) mv {inConditions = [cond "MV-1" "KM" "6000" 80000, fee], inPosted = ahead}
  check "Einmalzahlung zur Fälligkeit" [("BG", d 2026 3 15)] (originals feeDocs)
  check "Einmalzahlung: keine Nachberechnung" [] [es | (_, _, es) <- corr feeDocs, not (null es)]

  -- Rückwirkend zum 31.01. beendet: Februar gutschreiben, Konto aus dem Vermerk
  let ended = mv {inContracts = [c {cTo = d 2026 1 31} | c <- inContracts mv], inConditions = [cond "MV-1" "KM" "6000" 80000], inPosted = janFeb}
      (endDocs, _) = run ended
  check "beendet: Februar gutgeschrieben" [(d 2026 2 20, "DG", [("KM", d 2026 2 1, -80000)])] (corr endDocs)
  check "beendet: Erlöskonto aus dem Vermerk" ["6000"] [lAccount l | x <- endDocs, l <- dLines x, lAccount l /= "1200"]

  -- Kondition entfallen (gelöscht): Vermerktes gutschreiben
  let (gone, _) = run mv {inConditions = [], inPosted = janFeb}
  check "entfallen: beide Monate gutgeschrieben" [-80000, -80000] [a | (_, _, es) <- corr gone, (_, _, a) <- es]

  -- Fälligkeitstag auf den 20. verschoben: Februar wäre erst nach dem Stichtag fällig – trotzdem keine Gutschrift
  let (moved, _) = plan (d 2026 2 10) mv {inConditions = [(cond "MV-1" "KM" "6000" 80000) {kDueDay = 20}], inPosted = janFeb}
  check "Fälligkeitstag verschoben: nichts" 0 (length moved)

  -- Alte Vermerke ohne Periode (vor 0.4.0): Periode aus dem Rhythmus der Kondition
  let old = [p {pPeriod = Nothing} | p <- janFeb]
      (oldDocs, _) = run mv {inConditions = [cond "MV-1" "KM" "6000" 75000], inPosted = old}
  check "alte Vermerke: Gutschrift je Monat" [-5000, -5000] [a | (_, _, es) <- corr oldDocs, (_, _, a) <- es]

  -- Nachberechnung und neue Fälligkeit desselben Tages: ein Beleg, Saldo bestimmt die Belegart
  let (mixed, _) = plan (d 2026 3 10) mv {inConditions = [cond "MV-1" "KM" "6000" 75000], inPosted = janFeb, inCorrectionDate = d 2026 3 3}
  check "gemischt: ein Beleg am 03.03. als Rechnung" [(d 2026 3 3, "DR")] [(dDate x, dDocumentType x) | x <- mixed]

  -- Rückwirkend buchen ab 01.01.: Nachberechnung zur ursprünglichen Fälligkeit statt zum Lauftag
  let (back, _) = plan (d 2026 2 28) mv {inConditions = [cond "MV-1" "KM" "6000" 75000], inPosted = janFeb, inBackdateFrom = Just (d 2026 1 1)}
  check "rückwirkend: Gutschriften zu den Fälligkeiten" [d 2026 1 3, d 2026 2 3] (sort [dDate x | x <- back])
  let (limit, _) = plan (d 2026 2 28) mv {inConditions = [cond "MV-1" "KM" "6000" 75000], inPosted = janFeb, inBackdateFrom = Just (d 2026 2 1)}
  check "rückwirkend: nicht vor der Grenze" [d 2026 2 1, d 2026 2 3] (nub (sort [dDate x | x <- limit]))

  -- Kreditorenvertrag: Gutschrift mit der Belegart der Vertragsart
  let vs = input {inContracts = [inContracts input !! 1], inConditions = [cond "VS-1" "EN" "7300" 10000], inPosted = [booked "VS-1" "EN" "7300" 2026 1 12000]}
  check "Kreditor: Gutschrift-Belegart der Vertragsart" ["KX"] [dDocumentType x | x <- fst (plan (d 2026 1 31) vs)]

-- | Tilgungsplan: Zinsmethoden, Annuität, Sondertilgung, Zinswechsel,
-- Ratentilgung, endfällig mit Vertragsende, Übernahme; Einbindung in die Planung.
loanTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
loanTests check = do
  let d = fromGregorian
      terms =
        LoanTerms
          { lnContract = "DA-1", lnFrom = d 2026 1 30, lnTo = openEnd, lnPrincipal = 10000000, lnStart = d 2026 1 30, lnTakeover = False
          , lnRepayment = Annuity, lnRate = 36 / 10, lnInstallment = 500000, lnFrequency = Monthly, lnDueDay = 30, lnDayCount = D30360
          , lnLoanAccount = "3150", lnInterestAccount = "7310", lnInterestType = "DZ", lnPrincipalType = "DT", lnSpecialType = "DS"
          , lnDisbursementType = "AZ"
          }
      plan' ts ps end h = schedule ts ps end h
      regular = filter ((== Regular) . rKind)
  check "30/360: 31.01.–28.02. (Monatsende = 30.)" (30 / 360) (yearFraction D30360 (d 2026 1 31) (d 2026 2 28))
  check "30/360: 15.01.–15.03." (60 / 360) (yearFraction D30360 (d 2026 1 15) (d 2026 3 15))
  check "30/360: 28.02.–30.03." (30 / 360) (yearFraction D30360 (d 2026 2 28) (d 2026 3 30))
  check "act/360" (29 / 360) (yearFraction ACT360 (d 2026 1 30) (d 2026 2 28))
  check "Zinssatz" (Just (69 / 20)) (parseRate "3,45")

  -- Annuität 5.000,00 bei 100.000,00 und 3,6 %: Zins 300,00, Tilgung 4.700,00
  let a = plan' [terms] [] openEnd (d 2026 4 30)
  check "Auszahlung" [Row Disbursement (d 2026 1 30) (d 2026 1 30) (d 2026 1 30) 0 10000000 10000000] (take 1 a)
  check "Annuität: Zins" [30000, 28590, 27176] (map rInterest (regular a))
  check "Annuität: Tilgung" [470000, 471410, 472824] (map rPrincipal (regular a))
  check "Annuität: Fälligkeiten" [d 2026 2 28, d 2026 3 30, d 2026 4 30] (map rDue (regular a))
  check "Annuität: Restschuld" 8585766 (rBalance (last a))

  -- Sondertilgung 10.000,00 am 15.03.: Zins bis dahin auf die alte, danach auf die neue Restschuld
  let s = plan' [terms] [LoanPayment "DA-1" (d 2026 3 15) 1000000] openEnd (d 2026 3 31)
  check "Sondertilgung: Zeile" [(Special, 1000000, 8530000)] [(rKind r, rPrincipal r, rBalance r) | r <- s, rKind r == Special]
  -- 9.530.000 × 0,3 % × 15/30 + 8.530.000 × 0,3 % × 15/30 = 14.295 + 12.795
  check "Sondertilgung: Zins März" [27090] [rInterest r | r <- regular s, rDue r == d 2026 3 30]

  -- Zinswechsel ab 01.04. auf 4,8 % (act/360): Periode 31.03.–30.04. – der 31.03. zum alten, 30 Tage zum neuen Satz
  let act = terms {lnDayCount = ACT360}
      changed = plan' [act, act {lnFrom = d 2026 4 1, lnRate = 48 / 10}] [] openEnd (d 2026 4 30)
      march = rBalance (regular changed !! 1)
  check "Zinswechsel: April anteilig" (roundTo (fromInteger march * (36 / 1000 * 1 / 360 + 48 / 1000 * 30 / 360)))
    (rInterest (last (regular changed)))
  check "Zinswechsel: März unverändert" (map rInterest (take 2 (regular (plan' [act] [] openEnd (d 2026 3 30)))))
    (map rInterest (take 2 (regular changed)))

  -- Ratentilgung: Tilgung fest, Zins dazu
  let inst = plan' [terms {lnRepayment = Instalment, lnInstallment = 100000}] [] openEnd (d 2026 3 30)
  check "Ratentilgung" [(30000, 100000), (29700, 100000)] [(rInterest r, rPrincipal r) | r <- regular inst]

  -- Endfällig mit Vertragsende: nur Zinsen, am Ende die Restschuld
  let bullet = plan' [terms {lnRepayment = Bullet, lnInstallment = 0}] [] (d 2026 4 30) (d 2030 1 1)
  check "endfällig" [(30000, 0), (30000, 0), (30000, 10000000)] [(rInterest r, rPrincipal r) | r <- regular bullet]
  check "endfällig: getilgt" 0 (rBalance (last bullet))

  -- Übernahme: keine Auszahlung
  check "Übernahme ohne Auszahlung" [] [r | r <- plan' [terms {lnTakeover = True}] [] openEnd (d 2026 3 1), rKind r == Disbursement]

  -- Einbindung: Darlehensvertrag (wir zahlen) – Auszahlung, Zins, Tilgung gegen die Bank
  let bank = Contract "1000" "DA-1" "DA" "ACTIVE" "BANK" "EUR" "" (d 2026 1 1) openEnd Nothing
      input =
        Input
          { inTypes = Map.fromList [("DA", ContractType "DA" "PAYABLE" "CREDITOR" (Just "KR") Nothing True)]
          , inContracts = [bank], inConditions = [], inObjects = [], inMeasurements = [], inAccounts = Map.empty, inFieldStatus = Map.empty
          , inLoans = [terms], inLoanPayments = [], inDepreciations = []
          , inPartnerAccounts = Map.fromList [(("BANK", "CREDITOR"), PartnerAccount "BANK" "CREDITOR" (Just "1600") False)]
          , inPosted = [], inCorrectionDate = d 2026 3 10, inBackdateFrom = Nothing, inConditionTypes = Map.empty, inDependentTypes = Map.empty, inTaxCodes = Map.empty, inRoundings = Map.empty, inCompanyRounding = Nothing, inDecimals = Map.empty
          }
      (docs, msgs) = plan (d 2026 2 28) input
  check "Darlehen: keine Meldungen" [] msgs
  check "Darlehen: Auszahlung Bank an Darlehen" (sort [("1600", "S", 10000000), ("3150", "H", 10000000)])
    (sort [(lAccount l, lSide l, lAmount l) | x <- docs, dDate x == d 2026 1 30, l <- dLines x])
  check "Darlehen: Rate Zins und Tilgung an Bank" (sort [("7310", "S", 30000), ("1600", "H", 30000), ("3150", "S", 470000), ("1600", "H", 470000)])
    (sort [(lAccount l, lSide l, lAmount l) | x <- docs, dDate x == d 2026 2 28, l <- dLines x])
  where
    roundTo :: Rational -> Integer
    roundTo x = roundDiv (numerator x) (denominator x)

-- | Vertragsabrechnung (WEG): Prüfungen und Buchungsplan.
-- | Abschreibung: linear und degressiv monatsgenau, Zugang, Verkauf; Buchung nur auf Sachkonten.
depreciationTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
depreciationTests check = do
  let d = fromGregorian
      terms = DepTerms "KV-1" (d 2025 7 15) openEnd 30000000 2 Linear (d 2025 7 15) Nothing "0230" "6221" "AF"
      rows = depSchedule [terms] openEnd (d 2027 12 31)
  check "linear: 2025 sechs Monate, dann volle Jahre" [(d 2025 12 31, 6, 300000), (d 2026 12 31, 12, 600000), (d 2027 12 31, 12, 600000)]
    [(drDue r, drMonths r, drAmount r) | r <- rows]
  check "linear: Buchwert" [29700000, 29100000, 28500000] (map drBookAfter rows)
  -- Übernahme mit Restbuchwert, nachträgliche Herstellungskosten 2027 (+10.000)
  let taken = terms {dpStart = d 2026 1 1, dpFrom = d 2026 1 1, dpOpening = Just 29700000}
      later = taken {dpFrom = d 2027 3 1, dpBase = 31000000}
      rows2 = depSchedule [taken, later] openEnd (d 2027 12 31)
  check "Übernahme und Zugang" [(600000, 0, 29100000), (620000, 1000000, 29480000)] [(drAmount r, drAddition r, drBookAfter r) | r <- rows2]
  -- degressiv 5 % vom Buchwert, Verkauf zum 30.06.2027
  let degr = terms {dpMethod = Degressive, dpRate = 5, dpStart = d 2026 1 1, dpFrom = d 2026 1 1}
      rows3 = depSchedule [degr] (d 2027 6 30) (d 2030 12 31)
  check "degressiv und Verkauf" [(d 2026 12 31, 1500000), (d 2027 6 30, 712500)] [(drDue r, drAmount r) | r <- rows3]
  -- Plan: Kaufvertrag ohne Partnerkonto, Beleg SA, Aufwand an Anlage, Gebäude als Kontierung
  let kv = Contract "1000" "KV-1" "KV" "ACTIVE" "SELLER" "EUR" "" (d 2025 7 15) openEnd Nothing
      input =
        Input
          { inTypes = Map.fromList [("KV", ContractType "KV" "PAYABLE" "CREDITOR" Nothing Nothing False)]
          , inContracts = [kv], inConditions = [], inObjects = [ContractObject "KV-1" "Building" "HAUS1" True (d 2025 7 15) openEnd]
          , inMeasurements = [], inAccounts = Map.empty, inFieldStatus = Map.empty, inLoans = [], inLoanPayments = []
          , inDepreciations = [terms], inPartnerAccounts = Map.empty, inPosted = [], inCorrectionDate = d 2026 12 31, inBackdateFrom = Nothing, inConditionTypes = Map.empty, inDependentTypes = Map.empty, inTaxCodes = Map.empty, inRoundings = Map.empty, inCompanyRounding = Nothing, inDecimals = Map.empty
          }
      (docs, msgs) = plan (d 2026 12 31) input
  check "Abschreibung: keine Meldungen" [] msgs
  check "Abschreibung: zwei Belege SA" [(d 2025 12 31, "SA"), (d 2026 12 31, "SA")] [(dDate x, dDocumentType x) | x <- docs]
  check "Abschreibung: Aufwand an Anlage, Gebäude"
    [("6221", "S", 600000, Just "HAUS1"), ("0230", "H", 600000, Just "HAUS1")]
    [(lAccount l, lSide l, lAmount l, lookup "dimension_custom_1" (lFields l)) | x <- docs, dDate x == d 2026 12 31, l <- dLines x]

settlementTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
settlementTests check = do
  let d = fromGregorian
      h =
        SetHead
          { shFrom = d 2025 1 1, shTo = d 2025 12 31, shDate = d 2026 5 10, shPosting = d 2026 5 10, shReference = "WEG-2025"
          , shStatedAdvances = Just 360000, shStatedResult = Just (-152203), shReserveOpening = Just 420000, shReserveWithdrawal = 0
          , shReserveClosing = Just 480000, shReserveAccount = Nothing, shWithdrawalAccount = Nothing, shStatus = "OPEN"
          }
      item n cat typ total kt ks amt acc = SetItem n "" cat typ total (if kt == Nothing then "" else "MEA") kt ks amt acc "RentObject" "WG001"
      water = item 1 "WASSER" "ALLOCABLE" (Just 1240000) (Just 1000) (Just (8532 / 100)) 105797 "7000"
      items = [water, item 2 "VERWALT" "NON_ALLOCABLE" Nothing Nothing Nothing 42000 "7300", item 3 "RUECKL" "RESERVE" Nothing Nothing Nothing 60000 "1550"]
      prev = Just (h {shFrom = d 2024 1 1, shTo = d 2024 12 31, shReserveClosing = Just 420000}, items)
      level name cs = [chLevel c | c <- cs, chName c == name]
      cs = checks 5 h items 360000 prev (Just 420000)
  check "Einzelbetrag aus Schlüssel (12.400 × 85,32 / 1000)" (Just 105797) (expectedShare water)
  check "Prüfungen ok" [] [(chName c, chLevel c) | c <- cs, chLevel c `notElem` [Ok, Info]]
  check "Zu buchen: Guthaben" [Just (-152203)] [chActual c | c <- cs, chName c == "Zu buchen"]

  let off = checks 5 h [water {siAmount = 105900}] 350000 Nothing (Just 400000)
  check "Abweichung Einzelbetrag" [Deviation] (level "Position 1 WASSER" off)
  check "Abweichung Vorauszahlungen" [Deviation] (level "Vorauszahlungen" off)
  check "Abweichung Rücklage Hauptbuch" [Deviation] (level "Rücklage Hauptbuch" off)
  check "Fehlende Kostenart des Vorjahres" [Info] (level "Kostenart VERWALT" (checks 5 h [water, last items] 360000 prev (Just 420000)))
  check "Ohne Positionen: Fehler" [Failure] (level "Positionen" (checks 5 h [] 0 Nothing Nothing))
  check "Innerhalb der Toleranz" [Ok] (level "Position 1 WASSER" (checks 5 h [water {siAmount = 105800}] 0 Nothing Nothing))
  check "Gebucht: Hauptbuch enthält die Zuführung" [Ok] (level "Rücklage Hauptbuch" (checks 5 h {shStatus = "POSTED"} items 360000 Nothing (Just 480000)))

  -- Buchungsplan: Kosten Soll, Vorauszahlungen Haben, Guthaben Soll Kreditor
  case settlementLines "Abrechnung 2025" h items [("1590", 300000), ("1591", 60000)] "2900" of
    Left e -> check "Buchungsplan" "" e
    Right ls -> do
      check "Plan: Kosten im Soll" [("7000", "S", 105797), ("7300", "S", 42000), ("1550", "S", 60000)]
        [(plAccount l, plSide l, plAmount l) | l <- take 3 ls]
      check "Plan: Vorauszahlungen im Haben" [("1590", "H", 300000), ("1591", "H", 60000)] [(plAccount l, plSide l, plAmount l) | l <- take 2 (drop 3 ls)]
      check "Plan: Guthaben Soll Kreditor" [("2900", "S", 152203, True)] [(plAccount l, plSide l, plAmount l, plPartner l) | l <- drop 5 ls]
      check "Plan: ausgeglichen" 0 (sum [if plSide l == "S" then plAmount l else negate (plAmount l) | l <- ls])
  -- Nachzahlung und Entnahme
  let hw = h {shReserveWithdrawal = 50000, shWithdrawalAccount = Just "7100"}
  case settlementLines "A" hw items [("1590", 100000)] "2900" of
    Left e -> check "Plan mit Entnahme" "" e
    Right ls -> do
      check "Entnahme: Gegenkonto Soll, Rücklage Haben" [("7100", "S", 50000), ("1550", "H", 50000)]
        [(plAccount l, plSide l, plAmount l) | l <- ls, plText l == "Entnahme Rücklage A"]
      check "Nachzahlung Haben Kreditor" [("2900", "H", 107797)] [(plAccount l, plSide l, plAmount l) | l <- ls, plPartner l]
  check "Entnahme ohne Gegenkonto" (Left "Entnahme aus der Rücklage: Gegenkonto fehlt")
    (either Left (const (Right ())) (settlementLines "A" h {shReserveWithdrawal = 1} items [] "2900"))

-- | Nebenkostenabrechnung: Sammeln mit Kontierung, Umbuchen, Verteilen nach
-- Personentagen und Wohnfläche, Leerstand, Rundung auf die Mieter.
opCostTests :: (forall a. (Eq a, Show a) => String -> a -> a -> IO ()) -> IO ()
opCostTests check = do
  let d = fromGregorian
      y = (d 2025 1 1, d 2025 12 31)
      rule no step kind src val pool cat key =
        Rule no step "" kind cat src val pool "" 0 key True "SERVICE_PERIOD"
      rules =
        [ rule 10 1 "COLLECT" "LEDGER" "6300" "WASSER" "Wasser" ""
        , rule 20 1 "COLLECT" "INVOICE" "GRUNDSTEUER" "GRST" "Grundsteuer" ""
        , rule 30 1 "COLLECT" "INVOICE" "MESSDIENST" "HEIZ" "Heizung" ""
        , (rule 40 2 "TRANSFER" "" "" "WASSER" "" "") {ruToPool = "ABW", ruShare = 30}
        , rule 50 3 "DISTRIBUTE" "" "" "WASSER" "Wasser" "PERS"
        , rule 60 3 "DISTRIBUTE" "" "" "ABW" "Abwasser" "WFL"
        , rule 70 3 "DISTRIBUTE" "" "" "GRST" "Grundsteuer" "DIREKT"
        ]
      keys =
        Map.fromList
          [ (akCode k, k)
          | k <- [AllocKey "PERS" "Personen" "PERSONS" Nothing, AllocKey "WFL" "Wohnfläche" "MEASUREMENT" (Just "WFL"), AllocKey "DIREKT" "direkt" "DIRECT" Nothing]
          ]
      inp =
        OpInput
          { opDef = Def "WE1" 100000 "TENANTS_OR_OWNER" (fst y) (snd y)
          , opRules = rules
          , opKeys = keys
          , opObjects = ["W1", "W2"]
          , opMeasurements = [Measurement "W1" "WFL" 60 (d 2020 1 1) openEnd, Measurement "W2" "WFL" 40 (d 2020 1 1) openEnd]
          , opOccupancy = [Occupancy "MV1" "P1" "W1" (d 2025 1 1) (d 2025 12 31), Occupancy "MV2" "P2" "W2" (d 2025 7 1) (d 2025 12 31)]
          , opPersons = [PersonSlice "MV1" 2 (d 2024 1 1) openEnd, PersonSlice "MV2" 1 (d 2025 7 1) openEnd]
          , opSources =
              [ Source "LEDGER" "6300" "JE:1" 100000 ToPool "" "" 10 Nothing
              , Source "INVOICE" "GRUNDSTEUER" "INV:1" 30000 (ToObject "W1") "" "" 20 Nothing
              , Source "INVOICE" "MESSDIENST" "INV:2" 2500 (ToTenant "MV2") "" "" 30 (Just "Rechnung 2 ohne Leistungszeitraum")
              ]
          , opAdvances = Map.fromList [("MV1", 120000)]
          , opCategories = Map.fromList [("Wasser", "Wasserversorgung")]
          , opCoverage = []
          }
      res = computeOpCost inp
      tenant c = [(trCosts t, trAdvances t, trLines t) | t <- orTenants res, trContract t == c]
  check "NK: Summe der Quellen" 132500 (orSources res)
  -- Wasser 700,00 nach Personentagen 730/914 bzw. 184/914, Abwasser 300,00 nach Wohnfläche 60/40,
  -- W2 zu 184/365 an MV2, 181/365 Leerstand; Rundung 2 Cent: je Mieter 1 Cent
  check "NK: Mietobjekt MV2" ["W2"] [trObject t | t <- orTenants res, trContract t == "MV2"]
  check "NK: MV1" [(103909, 120000, [("Abwasser", 18000), ("Grundsteuer", 30000), ("Rundung", 1), ("Wasserversorgung", 55908)])] (tenant "MV1")
  check "NK: MV2" [(22641, 0, [("Abwasser", 6049), ("Heizung", 2500), ("Rundung", 1), ("Wasserversorgung", 14091)])] (tenant "MV2")
  check "NK: Eigentümer (Leerstand W2)" 5950 (orOwnerTotal res)
  check "NK: Rundung" 2 (orRounding res)
  check "NK: Hinweis ohne Leistungszeitraum" ["Regel 30"] [chName c | c <- orChecks res, chLevel c `elem` [Failure, Deviation]]
  check "NK: Journal ausgeglichen" True (all ((> 0) . jAmount) (orJournal res))
  -- Rest < Anzahl Mieter → Eigentümer; ohne Verteilregel bleibt der Topf offen
  let one = computeOpCost inp {opRules = take 1 rules <> [rule 50 3 "DISTRIBUTE" "" "" "WASSER" "Wasser" "WFL"], opSources = take 1 (opSources inp)}
  check "NK: Wasser nach WFL, W2 mit Leerstand" [60000] [trCosts t | t <- orTenants one, trContract t == "MV1"]
  -- W2: 400,00 × 184/365 = 201,64… an MV2, Leerstand 198,35…; 1 Cent Rest < 2 Mieter → Eigentümer
  check "NK: Rundung 1 Cent" 1 (orRounding one)
  check "NK: Rest an den Eigentümer" (19835 + 1) (orOwnerTotal one)
  check "NK: MV2 zeitanteilig" [20164] [trCosts t | t <- orTenants one, trContract t == "MV2"]
  let open' = computeOpCost inp {opRules = take 3 rules}
  check "NK: Töpfe offen" ["Offen O:W1:GRST", "Offen S:WASSER"] (sort [chName c | c <- orChecks open', chLevel c == Failure])
  check "NK: Summe weicht ab" [Deviation] [chLevel c | c <- orChecks open', chName c == "Summe"]

  -- Vollständigkeit der Vertragsabrechnungen
  check "Lücken" [(d 2025 1 1, d 2025 5 31), (d 2025 11 1, d 2025 12 31)] (uncovered y [(d 2025 6 1, d 2025 10 31)])
  check "keine Lücke" [] (uncovered y [(d 2024 6 1, d 2025 5 31), (d 2025 6 1, d 2026 5 31)])
  let cov = Coverage 10 "VE-1" y
      levels cs = [(chLevel c, chText c) | c <- coverageChecks cs]
  check "vollständig" [(Ok, "Zeitraum vollständig abgerechnet")] (levels [cov [y] True [] True])
  check "fehlt (Vorjahr abgerechnet)" [(Failure, "im Vorjahr abgerechnet – für den Zeitraum fehlt die Abrechnung")] (levels [cov [] True [] True])
  check "erfasst, nicht gebucht" [Failure] (map fst (levels [cov [] False ["01.01.2025–31.12.2025 (OPEN)"] True]))
  check "Teilzeitraum" [(Failure, "nicht abgerechnet: 01.06.2025–31.12.2025")] (levels [cov [(d 2025 1 1, d 2025 5 31)] True [] True])
  check "nach Datum ohne Lückenprüfung" [Ok] (map fst (levels [cov [(d 2025 3 1, d 2025 3 1)] True [] False]))
  let withCov = computeOpCost inp {opCoverage = [cov [] True [] True]}
  check "NK: Fehler aus Vollständigkeit" 1 (length [() | c <- orChecks withCov, chLevel c == Failure])
