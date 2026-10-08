{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
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

  -- Einmalig: am Beginn, voller Betrag
  check "einmalig" [Due (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) (d 2026 5 5) 777] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 6 1))
  check "einmalig, noch nicht fällig" [] (dues Once InAdvance 1 (fixed 777) (d 2026 5 5) (d 2026 12 31) (d 2026 5 4))
  -- Einmalig mit eigenem Fälligkeitsdatum (Bereitstellungsgebühr bei Übergabe)
  check "einmalig am Fälligkeitsdatum" [d 2026 6 1] (dates (dueOnce (Just (d 2026 6 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 6 30)))
  check "einmalig: Fälligkeit nach dem Stichtag" [] (dueOnce (Just (d 2026 6 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31))
  check "einmalig: Fälligkeit vor Beginn gilt ab Beginn" [d 2026 5 5] (dates (dueOnce (Just (d 2026 4 1)) (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31)))
  check "einmalig ohne Datum: am Beginn" [d 2026 5 5] (dates (dueOnce Nothing (fixed 5000) (d 2026 5 5) openEnd (d 2026 5 31)))

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
          [ ("MV", ContractType "MV" "RECEIVABLE" "TENANT" Nothing Nothing False)
          , ("VS", ContractType "VS" "PAYABLE" "CREDITOR" (Just "KR") (Just "KX") True)
          , ("XX", ContractType "XX" "RECEIVABLE" "GUARANTOR" Nothing Nothing False)
          ]
      contract' cid typ = Contract "1000" cid typ "ACTIVE" "P1" "EUR" "" (d 2026 1 1) openEnd
      cond cid typ acc amount =
        Condition cid typ "" False amount Nothing Monthly 3 Nothing InAdvance (Just acc) Nothing (d 2026 1 1) openEnd
      -- Vermerk (vorerfasst oder gebucht) einer ganzen Monatsperiode
      booked cid typ acc y m amount =
        Posted cid typ "" (d y m 1) (d y m (gregorianMonthLength y m)) (Just (d y m 1)) (d y m 3) amount Original acc (Just "WG001")
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
          , inPartnerAccounts =
              Map.fromList
                [ (("P1", "TENANT"), PartnerAccount "P1" "TENANT" (Just "1200") False)
                , (("P1", "CREDITOR"), PartnerAccount "P1" "CREDITOR" (Just "2900") False)
                , (("JC", "TENANT"), PartnerAccount "JC" "TENANT" (Just "1210") False)
                , (("ZZ", "TENANT"), PartnerAccount "ZZ" "TENANT" (Just "1200") True)
                ]
          , inPosted = [booked "MV-1" "KM" "SKR25-6000" 2026 1 80000]
          , inCorrectionDate = d 2026 2 20
          }
      (docs, msgs) = plan (d 2026 2 28) input
      docOf cid date = [x | x <- docs, cId (dContract x) == cid, dDate x == date]
  check "Meldung ohne Partnerkonto" [("XX-1", "Partnerkonto fehlt: P1 hat in der Rolle GUARANTOR keine Buchungskreisdaten (Partnermodul) – Vertrag nicht gebucht")] msgs
  -- Januar KM schon gebucht: die Minderung für Januar ist nachträglich (Nachberechnung am Lauftag), im Februar beides
  check "schon Gebuchtes übersprungen" 0 (length (docOf "MV-1" (d 2026 1 3)))
  check "Januar nachträglich" [[("MIN", Correction)]] [[(kType (eCondition e), eKind e) | e <- dEntries x] | x <- docOf "MV-1" (d 2026 2 20)]
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
  -- Überschneidende Zeitscheiben derselben Kondition: Meldung, kein Beleg
  let twice = input {inConditions = [cond "MV-1" "KM" "6000" 80000, (cond "MV-1" "KM" "6000" 85000) {kFrom = d 2026 2 1}], inPosted = mempty}
      (docs2, msgs2) = plan (d 2026 2 28) twice
  check "Überschneidung: kein Beleg für den Vertrag" 0 (length [x | x <- docs2, cId (dContract x) == "MV-1"])
  check "Überschneidung: Meldung" [("MV-1", "Kondition KM: Zeitscheiben überschneiden sich (01.02.2026) – Vertrag nicht gebucht")] [m | m@("MV-1", _) <- msgs2]

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
      janFeb = [booked "MV-1" "KM" "SKR25-6000" 2026 m 80000 | m <- [1, 2]]
      run inp = plan (d 2026 2 28) inp
      corr docs = [(dDate x, dDocumentType x, [(kType (eCondition e), duePeriodFrom (eDue e), dueAmount (eDue e)) | e <- dEntries x, eKind e == Correction]) | x <- docs]
      originals docs = [(kType (eCondition e), dueDate (eDue e)) | x <- docs, e <- dEntries x, eKind e == Original]

  -- Unverändert: nichts
  let (same, _) = run mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 80000], inPosted = janFeb}
  check "unverändert: kein Beleg" 0 (length same)

  -- Rückwirkende Minderung der Kaltmiete auf 750,00 ab Januar: zwei Gutschriften am Lauftag
  let (lower, _) = run mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 75000], inPosted = janFeb}
  check "Minderung: Gutschrift am Lauftag" [(d 2026 2 20, "DG", [("KM", d 2026 1 1, -5000), ("KM", d 2026 2 1, -5000)])] (corr lower)
  case lower of
    [x] -> do
      check "Minderung: Seiten getauscht" [("1200", "H", 5000), ("SKR25-6000", "S", 5000)] [(lAccount l, lSide l, lAmount l) | l <- take 2 (dLines x)]
      check "Minderung: Text" ["Nachberechnung KM 1/2026"] (map lText (take 1 (dLines x)))
    _ -> pure ()

  -- Mieterhöhung zum 15.02. rückwirkend: Februar anteilig nachbelasten (Rechnung DR)
  let raise = [(cond "MV-1" "KM" "SKR25-6000" 80000) {kTo = d 2026 2 14}, (cond "MV-1" "KM" "SKR25-6000" 94000) {kFrom = d 2026 2 15}]
      (higher, _) = run mv {inConditions = raise, inPosted = janFeb}
  check "Erhöhung: Differenz Februar 14/28 × 140,00" [(d 2026 2 20, "DR", [("KM", d 2026 2 1, 7000)])] (corr higher)

  -- Minderung als eigene Konditionsart, im März für Januar/Februar erfasst: Nachberechnung, nicht zur alten Fälligkeit
  let (late, _) = run mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 80000, (cond "MV-1" "MIN" "6000" (-10000)) {kTo = d 2026 2 28}], inPosted = janFeb}
  check "nachträglich erfasst: Nachberechnung" [(d 2026 2 20, "DG", [("MIN", d 2026 1 1, -10000), ("MIN", d 2026 2 1, -10000)])] (corr late)
  check "nachträglich erfasst: keine Sollstellung zur alten Fälligkeit" [] (originals late)

  -- Neue Kondition, die erst nach dem Lauftag fällig ist: zur Fälligkeit, auch wenn spätere Monate schon vorab gebucht sind
  let ahead = [booked "MV-1" "KM" "SKR25-6000" 2026 m 80000 | m <- [1 .. 4]]
      fee = (cond "MV-1" "BG" "6000" 15000) {kFrequency = Once, kFrom = d 2026 2 20, kDueDate = Just (d 2026 3 15)}
      (feeDocs, _) = plan (d 2026 4 30) mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 80000, fee], inPosted = ahead}
  check "Einmalzahlung zur Fälligkeit" [("BG", d 2026 3 15)] (originals feeDocs)
  check "Einmalzahlung: keine Nachberechnung" [] [es | (_, _, es) <- corr feeDocs, not (null es)]

  -- Rückwirkend zum 31.01. beendet: Februar gutschreiben, Konto aus dem Vermerk
  let ended = mv {inContracts = [c {cTo = d 2026 1 31} | c <- inContracts mv], inConditions = [cond "MV-1" "KM" "SKR25-6000" 80000], inPosted = janFeb}
      (endDocs, _) = run ended
  check "beendet: Februar gutgeschrieben" [(d 2026 2 20, "DG", [("KM", d 2026 2 1, -80000)])] (corr endDocs)
  check "beendet: Erlöskonto aus dem Vermerk" ["SKR25-6000"] [lAccount l | x <- endDocs, l <- dLines x, lAccount l /= "1200"]

  -- Kondition entfallen (gelöscht): Vermerktes gutschreiben
  let (gone, _) = run mv {inConditions = [], inPosted = janFeb}
  check "entfallen: beide Monate gutgeschrieben" [-80000, -80000] [a | (_, _, es) <- corr gone, (_, _, a) <- es]

  -- Fälligkeitstag auf den 20. verschoben: Februar wäre erst nach dem Stichtag fällig – trotzdem keine Gutschrift
  let (moved, _) = plan (d 2026 2 10) mv {inConditions = [(cond "MV-1" "KM" "SKR25-6000" 80000) {kDueDay = 20}], inPosted = janFeb}
  check "Fälligkeitstag verschoben: nichts" 0 (length moved)

  -- Alte Vermerke ohne Periode (vor 0.4.0): Periode aus dem Rhythmus der Kondition
  let old = [p {pPeriod = Nothing} | p <- janFeb]
      (oldDocs, _) = run mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 75000], inPosted = old}
  check "alte Vermerke: Gutschrift je Monat" [-5000, -5000] [a | (_, _, es) <- corr oldDocs, (_, _, a) <- es]

  -- Nachberechnung und neue Fälligkeit desselben Tages: ein Beleg, Saldo bestimmt die Belegart
  let (mixed, _) = plan (d 2026 3 10) mv {inConditions = [cond "MV-1" "KM" "SKR25-6000" 75000], inPosted = janFeb, inCorrectionDate = d 2026 3 3}
  check "gemischt: ein Beleg am 03.03. als Rechnung" [(d 2026 3 3, "DR")] [(dDate x, dDocumentType x) | x <- mixed]

  -- Kreditorenvertrag: Gutschrift mit der Belegart der Vertragsart
  let vs = input {inContracts = [inContracts input !! 1], inConditions = [cond "VS-1" "EN" "7300" 10000], inPosted = [booked "VS-1" "EN" "7300" 2026 1 12000]}
  check "Kreditor: Gutschrift-Belegart der Vertragsart" ["KX"] [dDocumentType x | x <- fst (plan (d 2026 1 31) vs)]
