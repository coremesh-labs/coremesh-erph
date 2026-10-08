-- | Vertragsabrechnung (Versorger, Grundsteuer, WEG-Jahresabrechnung) –
-- Prüfungen und Buchungsplan, reine Funktionen.
--
-- Prüfungen (Ampel: ok, Abweichung, Hinweis, Fehler):
--
--   * je Position mit Gesamtkosten und Schlüssel: Einzelbetrag = Gesamtkosten
--     × Anteil / Gesamt (kaufmännisch gerundet) innerhalb der Toleranz;
--   * Vorauszahlungen laut Abrechnung = gebuchte Vorauszahlungen des Zeitraums;
--   * Ergebnis laut Abrechnung = Summe der Einzelbeträge − Vorauszahlungen
--     laut Abrechnung;
--   * Rücklage: Anfang + Zuführung − Entnahme = Ende; Anfang = Ende der
--     Vorjahresabrechnung; Ende = Saldo des Rücklagenkontos im Hauptbuch
--     (vor dem Buchen: Saldo + Zuführung − Entnahme);
--   * Positionen ohne Betrag, Kostenarten des Vorjahres, die fehlen.
--
-- Buchungsplan (wir zahlen): Soll je Position (Aufwand bzw. Rücklagenkonto),
-- Haben die gebuchten Vorauszahlungen je Konto, Entnahme (Soll Gegenkonto,
-- Haben Rücklage), Differenz an das Partnerkonto (Nachzahlung Haben,
-- Guthaben Soll).
module Billing.Settlement
  ( SetHead (..)
  , SetItem (..)
  , Level (..)
  , Check (..)
  , PlanLine (..)
  , checks
  , settlementLines
  , reserveContribution
  , itemTotal
  , expectedShare
  ) where

import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)

import Billing.Schedule (roundDiv)

data SetHead = SetHead
  { shFrom :: Day
  , shTo :: Day
  , shDate :: Day
  , shPosting :: Day
  , shReference :: Text
  , shStatedAdvances :: Maybe Integer
  , shStatedResult :: Maybe Integer
  , shReserveOpening :: Maybe Integer
  , shReserveWithdrawal :: Integer
  , shReserveClosing :: Maybe Integer
  , shReserveAccount :: Maybe Text
  , shWithdrawalAccount :: Maybe Text
  , shStatus :: Text
  }
  deriving stock (Show)

data SetItem = SetItem
  { siLine :: Int
  , siGroup :: Text
  , siCategory :: Text
  , siCostType :: Text -- ^ ALLOCABLE | NON_ALLOCABLE | RESERVE
  , siTotal :: Maybe Integer
  , siKey :: Text
  , siKeyTotal :: Maybe Rational
  , siKeyShare :: Maybe Rational
  , siAmount :: Integer
  , siAccount :: Text
  , siObjectType :: Text
  , siObject :: Text
  }
  deriving stock (Show)

data Level = Ok | Deviation | Info | Failure
  deriving stock (Eq, Ord, Show)

data Check = Check
  { chName :: Text
  , chExpected :: Maybe Integer
  , chActual :: Maybe Integer
  , chLevel :: Level
  , chText :: Text
  }
  deriving stock (Eq, Show)

-- | Eine Zeile des Buchungsplans: Konto, Seite, Betrag (> 0), Text, Objekt der Kontierung.
data PlanLine = PlanLine
  { plAccount :: Text
  , plSide :: Text -- ^ S | H
  , plAmount :: Integer
  , plText :: Text
  , plObject :: Maybe (Text, Text) -- ^ Objektart, Objekt
  , plPartner :: Bool -- ^ Partnerkonto (Kreditor) – mit Partner kontieren
  }
  deriving stock (Eq, Show)

itemTotal :: [SetItem] -> Integer
itemTotal = sum . map siAmount

reserveContribution :: [SetItem] -> Integer
reserveContribution items = sum [siAmount i | i <- items, siCostType i == "RESERVE"]

-- | Einzelbetrag aus Gesamtkosten und Schlüssel, kaufmännisch gerundet.
expectedShare :: SetItem -> Maybe Integer
expectedShare i = do
  total <- siTotal i
  kt <- siKeyTotal i
  ks <- siKeyShare i
  if kt == 0
    then Nothing
    else let r = fromInteger total * ks / kt in Just (roundDiv (numerator r) (denominator r))

-- | Prüfungen. tol: Toleranz (kleinste Einheit); advances: gebuchte
-- Vorauszahlungen des Zeitraums; prev: Vorjahresabrechnung mit Positionen;
-- ledgerReserve: Saldo des Rücklagenkontos zum Ende des Zeitraums (ohne diese
-- Abrechnung, solange sie nicht gebucht ist).
checks :: Integer -> SetHead -> [SetItem] -> Integer -> Maybe (SetHead, [SetItem]) -> Maybe Integer -> [Check]
checks tol h items advances prev ledgerReserve =
  structural <> itemChecks <> advanceChecks <> reserveChecks <> completeness
  where
    within e a = abs (e - a) <= tol
    compareCheck name e a txt = Check name (Just e) (Just a) (if within e a then Ok else Deviation) txt
    structural =
      [Check "Positionen" Nothing Nothing Failure "Die Abrechnung hat keine Positionen" | null items]
        <> [Check ("Position " <> tshow (siLine i) <> " " <> siCategory i) Nothing (Just 0) Deviation "Einzelbetrag fehlt (0)" | i <- items, siAmount i == 0]
    itemChecks =
      [ compareCheck ("Position " <> tshow (siLine i) <> " " <> siCategory i <> groupText i) e (siAmount i) "Einzelbetrag = Gesamtkosten × Anteil / Gesamt"
      | i <- items
      , siAmount i /= 0
      , Just e <- [expectedShare i]
      ]
    groupText i = if siGroup i == "" then "" else " (" <> siGroup i <> ")"
    total = itemTotal items
    advanceChecks =
      [compareCheck "Vorauszahlungen" stated advances "laut Abrechnung = gebuchte Vorauszahlungen des Zeitraums" | Just stated <- [shStatedAdvances h]]
        <> [ compareCheck "Ergebnis" result (total - stated) "laut Abrechnung = Summe der Einzelbeträge − Vorauszahlungen laut Abrechnung"
           | Just result <- [shStatedResult h]
           , Just stated <- [shStatedAdvances h]
           ]
        <> [Check "Zu buchen" Nothing (Just (total - advances)) Info "Summe der Einzelbeträge − gebuchte Vorauszahlungen (Nachzahlung > 0, Guthaben < 0)"]
    contribution = reserveContribution items
    reserveChecks =
      [ compareCheck "Rücklage" closing (opening + contribution - shReserveWithdrawal h) "Ende = Anfang + Zuführung − Entnahme"
      | Just opening <- [shReserveOpening h]
      , Just closing <- [shReserveClosing h]
      ]
        <> [ compareCheck "Rücklage Vorjahr" pc opening "Anfang = Ende der Vorjahresabrechnung"
           | Just opening <- [shReserveOpening h]
           , Just (ph, _) <- [prev]
           , Just pc <- [shReserveClosing ph]
           ]
        <> [ compareCheck "Rücklage Hauptbuch" closing booked "Ende = Saldo des Rücklagenkontos im Hauptbuch"
           | Just closing <- [shReserveClosing h]
           , Just lb <- [ledgerReserve]
           , let booked = if shStatus h == "POSTED" then lb else lb + contribution - shReserveWithdrawal h
           ]
    completeness =
      [ Check ("Kostenart " <> c) Nothing Nothing Info "im Vorjahr abgerechnet, fehlt in dieser Abrechnung"
      | Just (_, pitems) <- [prev]
      , c <- nub (map siCategory pitems)
      , c `notElem` map siCategory items
      ]

-- | Buchungsplan (wir zahlen). advancesByAccount: gebuchte Vorauszahlungen je Konto;
-- partnerAccount: Abstimmkonto des Partners; label: Text (z. B. „Abrechnung 2025“).
settlementLines :: Text -> SetHead -> [SetItem] -> [(Text, Integer)] -> Text -> Either Text [PlanLine]
settlementLines label h items advancesByAccount partnerAccount = do
  reserveAcc <-
    if shReserveWithdrawal h == 0
      then Right Nothing
      else case shReserveAccount h of
        Just a -> Right (Just a)
        Nothing -> case [siAccount i | i <- items, siCostType i == "RESERVE"] of
          a : _ -> Right (Just a)
          [] -> Left "Entnahme aus der Rücklage: Rücklagenkonto fehlt (Kopf oder Rücklagen-Position)"
  withdrawalAcc <-
    if shReserveWithdrawal h == 0
      then Right Nothing
      else maybe (Left "Entnahme aus der Rücklage: Gegenkonto fehlt") (Right . Just) (shWithdrawalAccount h)
  let costLines =
        [ line (siAccount i) True (siAmount i) (siCategory i <> " " <> label) (obj i) False
        | i <- sortOn siLine items
        , siAmount i /= 0
        ]
      advanceLines =
        [ line acc False amt ("Vorauszahlungen " <> label) Nothing False
        | (acc, amt) <- Map.toList (Map.fromListWith (+) advancesByAccount)
        , amt /= 0
        ]
      w = shReserveWithdrawal h
      withdrawalLines = case (withdrawalAcc, reserveAcc) of
        (Just wa, Just ra) | w /= 0 -> [line wa True w ("Entnahme Rücklage " <> label) Nothing False, line ra False w ("Entnahme Rücklage " <> label) Nothing False]
        _ -> []
      diff = itemTotal items - sum (map snd advancesByAccount)
      partnerLines = [line partnerAccount False diff ((if diff > 0 then "Nachzahlung " else "Guthaben ") <> label) Nothing True | diff /= 0]
  pure (costLines <> advanceLines <> withdrawalLines <> partnerLines)
  where
    obj i = if siObject i == "" then Nothing else Just (siObjectType i, siObject i)
    -- debit: Betrag positiv = Soll; negativ tauscht die Seite
    line acc debit amt txt o p =
      let side
            | (amt >= 0) == debit = "S"
            | otherwise = "H"
       in PlanLine acc side (abs amt) txt o p

tshow :: Show a => a -> Text
tshow = T.pack . show
