-- | Nebenkostenabrechnung einer Abrechnungseinheit (Gebäude oder
-- Wirtschaftseinheit) – reine Funktionen.
--
-- Gerechnet wird doppelt auf internen Abrechnungskonten, ganzzahlig in
-- Cent × Faktor (Regelwerk, z. B. 100000):
--
--   * Q:<Quelle>:<Wert>   Quelle (Sachkonto, Kostenart der Rechnungen bzw. Vertragsabrechnungen)
--   * S:<Topf>            Topf der Einheit
--   * O:<Objekt>:<Topf>   Mietobjekt
--   * M:<Vertrag>:<Topf>  Mieter (Vertrag)
--   * T:<Vertrag>:<Objekt>:<Topf> Mieter je Mietobjekt (Verteilung über das Objekt,
--                         Eingabe je Wohnung) – Ausweis je Wohnung möglich
--   * E:<Objekt>:<Topf>   Eigentümer (Leerstand, Rundung)
--   * R:                  Rundung
--
-- Regeln in der Reihenfolge Schritt, Regel:
--
--   * Sammeln: Quelle → Topf; mit Kontierung direkt auf das Mietobjekt bzw.
--     den Mieter (z. B. Grundsteuer einer Wohnung, zusätzliche Anfahrt);
--   * Umbuchen: Anteil in % aller Konten eines Topfs in einen anderen Topf;
--   * Verteilen: Topf → Mietobjekte nach Schlüssel (Bemessung, Einheiten),
--     nach Personentagen direkt auf die Mieter, „direkt“ nur die Kontierung;
--     danach jedes Mietobjekt tagesgenau auf Mieter und Leerstand.
--
-- Jeder Anteil wird abgerundet, der Rest geht auf R:. Am Ende werden Mieter
-- und Eigentümer auf Cent abgerundet; die Rundung in Cent geht nach der
-- Rundungsregel an die Mieter (Rest ≥ Anzahl Mieter: je Mieter gleich viel,
-- der Rest an den Eigentümer) oder an den Eigentümer. Jede Buchung trägt ihre
-- Formel – das Journal ist der Rechenweg.
--
-- Danach die Ebenen (seit 0.19.0): Ebene 0 sind die Salden der Mieter je
-- Abrechnungskonto (Topf), je Wohnung, wenn das Konto „je Mietobjekt“
-- ausgewiesen wird, sonst je Vertrag. Die Regeln SUM (Summe von Konten),
-- ADVANCE (Vorauszahlungen) und BALANCE (A − B: Nachzahlung bzw. Guthaben)
-- erzeugen in ihrem Schritt die höheren Ebenen – gespeichert als
-- Abrechnung + Vertrag + Wohnung + Konto + Ebene, mit Formel.
module Billing.OpCost
  ( Def (..)
  , Rule (..)
  , AllocKey (..)
  , Target (..)
  , Source (..)
  , Occupancy (..)
  , PersonSlice (..)
  , OpInput (..)
  , Coverage (..)
  , coverageChecks
  , uncovered
  , Acc (..)
  , JLine (..)
  , TenantResult (..)
  , OpResult (..)
  , AccountDef (..)
  , ResLine (..)
  , PostLine (..)
  , Detail (..)
  , computeOpCost
  , accText
  , scaledText
  , overlap
  , days
  ) where

import Control.Monad (mfilter)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, addDays, diffDays)

import Billing.Data (Measurement (..))
import Billing.Plan (formatAmount, formatDay)
import Billing.Schedule (roundDiv)
import Billing.Settlement (Check (..), Level (..))

data Def = Def
  { dfUnit :: Text -- ^ Abrechnungseinheit (Eigentümerkonto der Rundung)
  , dfScale :: Integer -- ^ Faktor auf Cent
  , dfRounding :: Text -- ^ TENANTS_OR_OWNER | OWNER
  , dfFrom :: Day
  , dfTo :: Day
  , dfCostsOnly :: Bool -- ^ Kostenaufstellung je Einheit (Art COSTS): sammeln ohne Verteilung, keine Mieter
  }
  deriving stock (Show)

data Rule = Rule
  { ruNo :: Integer
  , ruStep :: Integer
  , ruDescription :: Text
  , ruKind :: Text -- ^ COLLECT | TRANSFER | DISTRIBUTE
  , ruCategory :: Text -- ^ Kostenart (Ausweis beim Mieter)
  , ruSourceType :: Text -- ^ LEDGER | INVOICE | CONTRACT_SETTLEMENT
  , ruSourceValue :: Text
  , ruPool :: Text
  , ruToPool :: Text
  , ruShare :: Rational -- ^ Prozent
  , ruKey :: Text
  , ruTimeSplit :: Bool -- ^ Leerstand tagesgenau an den Eigentümer (sonst nur auf die Mieter)
  , ruAssignment :: Text -- ^ Zuordnung der Quelle: SERVICE_PERIOD | POSTING_DATE | DOCUMENT_DATE
  , ruInputs :: [Text] -- ^ SUM: Konten; BALANCE: [A, B]
  , ruKeyTotal :: Text -- ^ DISTRIBUTE nach Bemessung: OBJECTS (Summe der Mietobjekte) | BUILDING (Bemessung des Gebäudes, z. B. MEA 1000 der WEG)
  }
  deriving stock (Show)

data AllocKey = AllocKey
  { akCode :: Text
  , akName :: Text
  , akBasis :: Text -- ^ MEASUREMENT | UNITS | PERSONS | CONSUMPTION | DIRECT
  , akMeasurement :: Maybe Text
  }
  deriving stock (Show)

-- | Kontierung einer Quelle.
data Target = ToPool | ToObject Text | ToTenant Text | ToTenantObject Text Text
  deriving stock (Eq, Show)

-- | Eine Kostenposition der Einheit im Zeitraum (schon zeitanteilig), in Cent.
data Source = Source
  { soType :: Text
  , soValue :: Text
  , soRef :: Text
  , soAmount :: Integer
  , soTarget :: Target
  , soFormula :: Text
  , soCategory :: Text
  , soRule :: Integer -- ^ Sammelregel, die die Quelle geladen hat
  , soWarning :: Maybe Text -- ^ z. B. ohne Leistungszeitraum nach Buchungsdatum zugeordnet
  }
  deriving stock (Show)

-- | Nutzung eines Mietobjekts durch einen Mietvertrag (im Zeitraum).
data Occupancy = Occupancy
  { ocContract :: Text
  , ocPartner :: Text
  , ocObject :: Text
  , ocFrom :: Day
  , ocTo :: Day
  }
  deriving stock (Show)

data PersonSlice = PersonSlice
  { psContract :: Text
  , psPersons :: Integer
  , psFrom :: Day
  , psTo :: Day
  }
  deriving stock (Show)

data OpInput = OpInput
  { opDef :: Def
  , opRules :: [Rule]
  , opKeys :: Map Text AllocKey
  , opObjects :: [Text] -- ^ Mietobjekte der Einheit
  , opMeasurements :: [Measurement]
  , opTotals :: [Measurement] -- ^ Bemessungen der Gebäude der Einheit (Gesamtschlüssel)
  , opOccupancy :: [Occupancy]
  , opPersons :: [PersonSlice]
  , opSources :: [Source]
  , opAdvances :: Map Text Integer -- ^ Vorauszahlungen je Vertrag (Cent)
  , opCategories :: Map Text Text -- ^ Kostenart → Bezeichnung (Ausweis beim Mieter)
  , opCoverage :: [Coverage] -- ^ Vollständigkeit der Vertragsabrechnungen je Sammelregel und Vertrag
  , opAccounts :: Map Text AccountDef -- ^ Abrechnungskonten (Bezeichnung, Ausweis)
  , opRuleValues :: Map Integer (Map Text Integer) -- ^ ADVANCE: Regel → Vertrag → Cent
  }

-- | Abrechnungskonto (Topf): Bezeichnung auf der Abrechnung, Ausweis je
-- Mietobjekt (OBJECT) oder je Vertrag (CONTRACT).
data AccountDef = AccountDef
  { adCode :: Text
  , adName :: Text
  , adBinding :: Text
  }
  deriving stock (Show)

-- | Ergebnis einer Ebene: Vertrag, Mietobjekt ("" = Vertrag), Konto, Ebene, Cent.
data ResLine = ResLine
  { rlContract :: Text
  , rlObject :: Text
  , rlAccount :: Text
  , rlLevel :: Integer
  , rlAmount :: Integer
  , rlFormula :: Text
  }
  deriving stock (Eq, Show)

-- | Buchung einer Saldo-Regel je Mieter: Kosten (A) und Vorauszahlungen (B).
data PostLine = PostLine
  { plRule :: Integer
  , plCost :: Integer
  , plAdvance :: Integer
  }
  deriving stock (Eq, Show)

-- | Vollständigkeit einer Quelle Vertragsabrechnung: welcher Teil des
-- Zeitraums (innerhalb der Vertragslaufzeit) durch gebuchte Abrechnungen
-- abgedeckt ist, ob der Vertrag im Vorjahr abgerechnet wurde und welche
-- Abrechnungen erfasst, aber nicht gebucht sind.
data Coverage = Coverage
  { cvRule :: Integer
  , cvContract :: Text
  , cvRequired :: (Day, Day)
  , cvCovered :: [(Day, Day)]
  , cvPrevious :: Bool
  , cvUnposted :: [Text]
  , cvGaps :: Bool -- ^ Lücken prüfen (Zuordnung nach Leistungszeitraum)
  }
  deriving stock (Show)

-- | F: Anteil der übrigen Eigentümer (Gesamtschlüssel des Gebäudes, z. B. WEG).
data Acc = Q Text Text | S Text | O Text Text | M Text Text | T Text Text Text | E Text Text | F Text | R
  deriving stock (Eq, Ord, Show)

accText :: Acc -> Text
accText = \case
  Q t v -> "Q:" <> t <> ":" <> v
  S p -> "S:" <> p
  O o p -> "O:" <> o <> ":" <> p
  M c p -> "M:" <> c <> ":" <> p
  T c o p -> "T:" <> c <> ":" <> o <> ":" <> p
  E o p -> "E:" <> o <> ":" <> p
  F p -> "F:" <> p
  R -> "R:"

poolOf :: Acc -> Maybe Text
poolOf = \case
  S p -> Just p
  O _ p -> Just p
  M _ p -> Just p
  T _ _ p -> Just p
  E _ p -> Just p
  F p -> Just p
  _ -> Nothing

withPool :: Text -> Acc -> Acc
withPool p = \case
  S _ -> S p
  O o _ -> O o p
  M c _ -> M c p
  T c o _ -> T c o p
  E o _ -> E o p
  F _ -> F p
  a -> a

data JLine = JLine
  { jStep :: Integer
  , jRule :: Integer
  , jDebit :: Acc
  , jCredit :: Acc
  , jAmount :: Integer -- ^ Cent × Faktor
  , jFormula :: Text
  , jSource :: Text
  , jCategory :: Text
  }
  deriving stock (Show)

data TenantResult = TenantResult
  { trContract :: Text
  , trPartner :: Text
  , trObject :: Text -- ^ Mietobjekt der Kontierung (meiste Nutzungstage)
  , trFrom :: Day
  , trTo :: Day
  , trCosts :: Integer -- ^ Cent
  , trAdvances :: Integer
  , trLines :: [(Text, Integer)] -- ^ Kostenart → Cent
  , trResults :: [ResLine] -- ^ Ebenen
  , trPost :: [PostLine]
  , trDetails :: [Detail]
  }
  deriving stock (Show)

data OpResult = OpResult
  { orJournal :: [JLine]
  , orTenants :: [TenantResult]
  , orChecks :: [Check]
  , orSources :: Integer -- ^ Summe der Quellen (Cent)
  , orTenantTotal :: Integer
  , orOwnerTotal :: Integer
  , orRounding :: Integer -- ^ Rundung in Cent (verteilt)
  , orForeignTotal :: Integer -- ^ Anteil der übrigen Eigentümer (Cent)
  , orPoolTotals :: Map Text Integer -- ^ Kosten je Topf der Einheit (Cent)
  }

-- --- Rechenzustand ----------------------------------------------------------------

data St = St
  { stBal :: Map Acc Integer
  , stJournal :: [JLine] -- ^ umgekehrt
  , stNotes :: [Check] -- ^ umgekehrt
  , stShares :: [Share] -- ^ Anteile der Verteilung (umgekehrt)
  }

-- | Anteil einer Verteilung: Ziel, Quelle, Gewicht / Summe, Schlüssel; Stufe
-- OBJECT (Topf → Mietobjekt) oder TENANT (Mietobjekt bzw. Topf → Mieter).
data Share = Share
  { shTo :: Acc
  , shFrom :: Acc
  , shWeight :: Rational
  , shTotal :: Rational
  , shKey :: Text
  , shStage :: Text
  }

-- | Rechenweg eines Mieterkontos der Ebene 0 (für das Abrechnungsschreiben):
-- Gesamtkosten des Topfs der Einheit, Schlüssel mit Gewicht und Summe,
-- Nutzung (Tage bzw. Personentage) und Faktor = Anteil des Mieters.
data Detail = Detail
  { dtAccount :: Text
  , dtObject :: Text -- ^ "" = Vertrag
  , dtTotal :: Integer -- ^ Cent, Kosten des Topfs der Einheit
  , dtKey :: Text
  , dtWeight :: Maybe Rational
  , dtWeightTotal :: Maybe Rational
  , dtUse :: Maybe Rational -- ^ Tage bzw. Personentage des Mieters
  , dtUseTotal :: Maybe Rational
  , dtUseKey :: Text
  , dtFactor :: Rational
  , dtAmount :: Integer -- ^ Cent
  }
  deriving stock (Eq, Show)

book :: JLine -> St -> St
book l st
  | jAmount l == 0 || jDebit l == jCredit l = st
  | otherwise =
      st
        { stBal = Map.insertWith (+) (jCredit l) (negate (jAmount l)) (Map.insertWith (+) (jDebit l) (jAmount l) (stBal st))
        , stJournal = l : stJournal st
        }

note :: Level -> Text -> Text -> St -> St
note lvl name txt st = st {stNotes = Check name Nothing Nothing lvl txt : stNotes st}

balance :: Acc -> St -> Integer
balance a = Map.findWithDefault 0 a . stBal

-- | Konten mit Saldo ≠ 0, die die Bedingung erfüllen.
accounts :: (Acc -> Bool) -> St -> [(Acc, Integer)]
accounts f st = [(a, b) | (a, b) <- Map.toList (stBal st), b /= 0, f a]

-- --- Rechnen ----------------------------------------------------------------------

computeOpCost :: OpInput -> OpResult
computeOpCost inp@OpInput {..} =
  OpResult
    { orJournal = journal
    , orTenants = tenants
    , orChecks = coverageChecks opCoverage <> reverse (stNotes final) <> endChecks
    , orSources = sourcesCents
    , orTenantTotal = sum (map trCosts tenants)
    , orOwnerTotal = ownerCents
    , orRounding = roundingCents
    , orForeignTotal = foreignCents
    , orPoolTotals = Map.map (`div` scale) poolTotals
    }
  where
    def = opDef
    scale = dfScale def
    rules = sortOn (\r -> (ruStep r, ruNo r)) opRules
    lastStep = maybe 0 ruStep (listToMaybe (reverse rules))
    afterRules = foldl' (flip (applyRule inp)) (St mempty [] [] []) rules
    (final, roundingCents) = roundToCents def (lastStep + 1) (tenantIds inp) afterRules
    journal = reverse (stJournal final)
    cents a = balance a final `div` scale
    sourcesCents = negate (sum [b | (Q _ _, b) <- Map.toList (stBal final)]) `div` scale
    ownerCents = sum [b `div` scale | (E _ _, b) <- Map.toList (stBal final)]
    -- übrige Eigentümer mit dem Rest unter einem Cent (ihre Bruchteile gehen nicht in die Rundung der Mieter)
    foreignCents = (sum [b | (F _, b) <- Map.toList (stBal final)] + balance R final) `div` scale
    labels = Map.union (Map.map adName opAccounts) (Map.map (\c -> Map.findWithDefault c c opCategories) (poolLabels rules))
    tenants =
      if dfCostsOnly def then [] else
      [ TenantResult
          { trContract = c
          , trPartner = partner
          , trObject = fst (maximumOn snd (Map.toList (Map.fromListWith (+) [(ocObject o, days (ocFrom o, ocTo o)) | o <- os])))
          , trFrom = minimum (map ocFrom os)
          , trTo = maximum (map ocTo os)
          , trCosts = sum (map snd ls)
          , trAdvances = if null advanceRules then Map.findWithDefault 0 c opAdvances else sum [rlAmount l | l <- rs, rlAccount l `elem` advancePools]
          , trLines = ls
          , trResults = rs
          , trPost = Map.findWithDefault [] c posts
          , trDetails = [d | (a, _) <- Map.toList (stBal final), Just (c', _, _) <- [tenantPool a], c' == c, Just d <- [detail a]]
          }
      | (c, os@(o0 : _)) <- Map.toList (Map.fromListWith (flip (<>)) [(ocContract o, [o]) | o <- opOccupancy])
      , let partner = ocPartner o0
            ls =
              [ (Map.findWithDefault p p labels, v)
              | (p, v) <- Map.toList (Map.fromListWith (+) [(p, cents a) | (a, _) <- Map.toList (stBal final), Just (c', _, p) <- [tenantPool a], c' == c])
              , v /= 0
              ]
            rs = [l | l <- resultLines, rlContract l == c]
      ]
    -- Kosten je Topf der Einheit: alles, was aus Quellen in den Topf gebucht wurde
    poolTotals = Map.fromListWith (+) [(p, jAmount l) | l <- journal, Q _ _ <- [jCredit l], Just p <- [poolOf (jDebit l)]]
    shares = stShares final
    shareTo a = listToMaybe [x | x <- shares, shTo x == a]
    detail a = case tenantPool a of
      Nothing -> Nothing
      Just (_, o, p) ->
        let tenantStep = shareTo a
            objectStep = if T.null o then Nothing else shareTo (O o p)
            -- Schlüssel: Topf → Mietobjekt, bei Personen Topf → Mieter
            keyStep = maybe (mfilter ((== "OBJECT") . shStage) tenantStep) Just objectStep
            ratio x = shWeight x / shTotal x
            factor = maybe 1 ratio objectStep * maybe 1 ratio tenantStep
            -- direkt beim Mieter (Eingabe, Kontierung): Faktor 1
         in Just
              Detail
                { dtAccount = p
                , dtObject = o
                , dtTotal = Map.findWithDefault 0 p poolTotals `div` scale
                , dtKey = maybe "" shKey keyStep
                , dtWeight = shWeight <$> keyStep
                , dtWeightTotal = shTotal <$> keyStep
                , dtUse = shWeight <$> mfilter ((== "TENANT") . shStage) tenantStep
                , dtUseTotal = shTotal <$> mfilter ((== "TENANT") . shStage) tenantStep
                , dtUseKey = maybe "" shKey (mfilter ((== "TENANT") . shStage) tenantStep)
                , dtFactor = factor
                , dtAmount = cents a
                }
    level0 = [(c, o, p, cents a) | (a, _) <- Map.toList (stBal final), Just (c, o, p) <- [tenantPool a], cents a /= 0]
    (resultLines, posts) = levels inp level0
    advanceRules = [r | r <- rules, ruKind r == "ADVANCE"]
    advancePools = map ruPool advanceRules
    leftover =
      [ Check ("Offen " <> accText a) Nothing (Just (b `div` scale)) Failure
          ("Kosten bleiben liegen: " <> scaledText scale b <> " – keine Verteilregel oder Schlüsselsumme 0")
      | (a, b) <- accounts isOpen final
      ]
    isOpen = \case
      S _ -> True
      O _ _ -> True
      _ -> False
    distributed = sum (map trCosts tenants) + ownerCents + foreignCents
    endChecks
      | dfCostsOnly def =
          [ Check "Kostenaufstellung" (Just sourcesCents) Nothing Info
              "ohne Verteilung: die Kosten bleiben bei der Einheit (Summe der Quellen)"
          ]
      | otherwise =
      leftover
        <> [ Check "Summe" (Just sourcesCents) (Just distributed) (if sourcesCents == distributed then Ok else Deviation)
               "Summe der Quellen = Mieter + Eigentümer (Leerstand, Rundung) + übrige Eigentümer"
           , Check "Rundung" Nothing (Just roundingCents) Info (roundingText def (length tenants) roundingCents)
           ]

-- | Mieterkonto: Vertrag, Mietobjekt ("" = Vertrag), Topf.
tenantPool :: Acc -> Maybe (Text, Text, Text)
tenantPool = \case
  M c p -> Just (c, "", p)
  T c o p -> Just (c, o, p)
  _ -> Nothing

-- | Ebenen: Ebene 0 aus den Mieterkonten (je Wohnung nur bei Ausweis „je
-- Mietobjekt“), dann SUM, ADVANCE und BALANCE in der Reihenfolge Schritt, Regel.
levels :: OpInput -> [(Text, Text, Text, Integer)] -> ([ResLine], Map Text [PostLine])
levels OpInput {..} level0 = (concat (reverse acc), Map.map reverse postsRev)
  where
    perObject p = maybe False ((== "OBJECT") . adBinding) (Map.lookup p opAccounts)
    base =
      [ ResLine c o p 0 v "Ebene 0 (Verteilung)"
      | ((c, o, p), v) <- Map.toList (Map.fromListWith (+) [((c, if perObject p then o else "", p), v) | (c, o, p, v) <- level0])
      ]
    contracts = Map.keys (Map.fromList [(ocContract o, ()) | o <- opOccupancy])
    resultRules = sortOn (\r -> (ruStep r, ruNo r)) [r | r <- opRules, ruKind r `elem` ["SUM", "ADVANCE", "BALANCE"]]
    (acc, postsRev) = foldl' step ([base], mempty) resultRules
    step (done, ps) r =
      let have = concat done
          get c p = [l | l <- have, rlContract l == c, rlAccount l == p]
          total c p = sum (map rlAmount (get c p))
          amt = formatAmount 2
       in case ruKind r of
            "SUM" ->
              let new =
                    [ ResLine c o (ruPool r) (ruStep r) (sum (map rlAmount ls)) (T.intercalate " + " [rlAccount l <> " " <> amt (rlAmount l) | l <- ls])
                    | c <- contracts
                    , (o, ls) <- Map.toList (Map.fromListWith (flip (<>)) [(if perObject (ruPool r) then rlObject l else "", [l]) | p <- ruInputs r, l <- get c p])
                    ]
               in (new : done, ps)
            "ADVANCE" ->
              let vals = Map.findWithDefault mempty (ruNo r) opRuleValues
                  new = [ResLine c "" (ruPool r) (ruStep r) v (ruDescription r) | c <- contracts, Just v <- [Map.lookup c vals]]
               in (new : done, ps)
            "BALANCE" -> case ruInputs r of
              [a, b] ->
                let one c =
                      let va = total c a
                          vb = total c b
                          d = va - vb
                          to = if d < 0 && not (T.null (ruToPool r)) then ruToPool r else ruPool r
                       in (ResLine c "" to (ruStep r) d (a <> " " <> amt va <> " − " <> b <> " " <> amt vb), PostLine (ruNo r) va vb)
                    rows = [one c | c <- contracts, not (null (get c a)) || not (null (get c b))]
                 in (map fst rows : done, foldl' (\m (l, pl) -> Map.insertWith (<>) (rlContract l) [pl] m) ps rows)
              _ -> (done, ps)
            _ -> (done, ps)

-- | Vertrag je Mieter der Einheit (Nutzung im Zeitraum).
tenantIds :: OpInput -> [Text]
tenantIds = Map.keys . Map.fromList . map (\o -> (ocContract o, ())) . opOccupancy

-- | Bezeichnung je Topf für den Ausweis beim Mieter: Kostenart der
-- Verteilregel, sonst der ersten Sammelregel, sonst der Topf.
poolLabels :: [Rule] -> Map Text Text
poolLabels rules =
  Map.insert "RUNDUNG" "Rundung" $
    Map.fromListWith (\_ old -> old) $
      [(ruPool r, ruCategory r) | r <- rules, ruKind r == "DISTRIBUTE", not (T.null (ruCategory r))]
        <> [(ruPool r, ruCategory r) | r <- rules, ruKind r == "COLLECT", not (T.null (ruCategory r))]

applyRule :: OpInput -> Rule -> St -> St
applyRule inp r = case ruKind r of
  "COLLECT" -> collect inp r
  "TRANSFER" -> transfer inp r
  "DISTRIBUTE" -> distribute inp r
  k | k `elem` ["SUM", "ADVANCE", "BALANCE"] -> id -- Ebenen nach der Verteilung
  k -> note Failure (ruleName r) ("unbekannte Art " <> k)

ruleName :: Rule -> Text
ruleName r = "Regel " <> tshow (ruNo r) <> (if T.null (ruDescription r) then "" else " " <> ruDescription r)

line :: Rule -> Acc -> Acc -> Integer -> Text -> Text -> Text -> JLine
line r = JLine (ruStep r) (ruNo r)

category :: Rule -> Text -> Text
category r fallback = if T.null (ruCategory r) then fallback else ruCategory r

collect :: OpInput -> Rule -> St -> St
collect OpInput {..} r st0
  | null mine = note Info (ruleName r) ("keine Kosten " <> ruSourceType r <> " " <> ruSourceValue r <> " im Zeitraum") st0
  | otherwise = foldl' (flip one) st0 mine
  where
    scale = dfScale opDef
    mine = [s | s <- opSources, soRule s == ruNo r]
    pool = ruPool r
    one s =
      let to = case soTarget s of
            ToPool -> S pool
            ToObject o -> O o pool
            ToTenant c -> M c pool
            ToTenantObject c o -> T c o pool
       in warn s . book (line r to (Q (soType s) (soValue s)) (soAmount s * scale) (soFormula s) (soRef s) (category r (soCategory s)))
    warn s = maybe id (note Deviation (ruleName r)) (soWarning s)

-- | Anteil aller Konten des Topfs in den Ziel-Topf (abgerundet).
transfer :: OpInput -> Rule -> St -> St
transfer OpInput {..} r st0 = foldl' (flip one) st0 (accounts ((== Just (ruPool r)) . poolOf) st0)
  where
    scale = dfScale opDef
    one (a, b) =
      let amt = floorR (fromInteger b * ruShare r / 100)
       in book (line r (withPool (ruToPool r) a) a amt (scaledText scale b <> " × " <> rationalText 4 (ruShare r) <> " %") "" (ruCategory r))

distribute :: OpInput -> Rule -> St -> St
distribute inp@OpInput {..} r st0 = case Map.lookup (ruKey r) opKeys of
  Nothing -> note Failure (ruleName r) ("Verteilerschlüssel " <> ruKey r <> " gibt es nicht") st0
  Just k -> case akBasis k of
    "MEASUREMENT" -> toTenants k (toObjects k (withOthers k (measured k)) st0)
    "UNITS" -> toTenants k (toObjects k (withOthers k [(o, 1) | o <- opObjects]) st0)
    "PERSONS" -> toTenants k (personsDirect k st0)
    "DIRECT" -> toTenants k st0
    b -> note Failure (ruleName r) ("Schlüssel " <> akCode k <> ": Grundlage " <> b <> " wird noch nicht gerechnet") st0
  where
    def = opDef
    scale = dfScale def
    period = (dfFrom def, dfTo def)
    pdays = days period
    pool = ruPool r
    cat = ruCategory r
    -- Bemessung je Objekt, zeitgewichtet über den Zeitraum
    measured k =
      [ (o, sum [toRational (mValue m) * fromInteger (days iv) | m <- opMeasurements, mObject m == o, Just (mType m) == akMeasurement k
                , Just iv <- [overlap period (mFrom m, mTo m)]] / fromInteger pdays)
      | o <- opObjects
      ]
    -- Gesamtschlüssel des Gebäudes (z. B. MEA 1000 der WEG): Rest = übrige Eigentümer
    withOthers k ws
      | ruKeyTotal r /= "BUILDING" = map (\(o, w) -> (O o pool, w, rationalText 4 w)) ws
      | otherwise =
          let total = sum [toRational (mValue m) * fromInteger (days iv) | m <- opTotals, Just (mType m) == akMeasurement k, Just iv <- [overlap period (mFrom m, mTo m)]] / fromInteger pdays
              own = sum (map snd ws)
           in [(O o pool, w, rationalText 4 w) | (o, w) <- ws] <> [(F pool, total - own, "übrige Eigentümer (Gesamt " <> rationalText 4 total <> ")") | total > own]
    -- Topf → Mietobjekte nach Gewicht
    toObjects k ws st =
      let a = balance (S pool) st
          missing = ruKeyTotal r == "BUILDING" && not (any (\(acc, _, _) -> acc == F pool) ws)
          st' = if missing && a /= 0 then note Deviation (ruleName r) ("Gesamtschlüssel " <> akName k <> ": keine größere Bemessung des Gebäudes – verteilt nur auf die Mietobjekte") st else st
       in if a == 0 then st else splitBook "OBJECT" (akName k) st' (S pool) a ws ("(" <> akName k <> ")")
    -- Topf → Mieter nach Personentagen
    personsDirect k st =
      let a = balance (S pool) st
          ws = [(M c pool, w, tshow w <> " Personentage") | (c, w) <- Map.toList (personDays inp Nothing)]
       in if a == 0 then st else splitBook "OBJECT" (akName k) st (S pool) a [(acc, fromInteger w, t) | (acc, w, t) <- ws] ("(" <> akName k <> ")")
    -- Mietobjekt → Mieter und Leerstand
    toTenants k st = foldl' (objectToTenants k) st (accounts isPoolObject st)
    isPoolObject = \case
      O _ p -> p == pool
      _ -> False
    objectToTenants k st (acc, b) = case acc of
      O o _ ->
        let occ = [x | x <- opOccupancy, ocObject x == o]
            tenantWeights
              | akBasis k == "PERSONS" =
                  [ (T c o pool, fromInteger w, tshow w <> " Personentage")
                  | (c, w) <- Map.toList (personDays inp (Just o)), w > 0
                  ]
              | otherwise =
                  [ (T c o pool, fromInteger n, tshow n <> " Tage " <> formatDay f <> "–" <> formatDay t)
                  | (c, (n, f, t)) <- Map.toList (Map.fromListWith merge [(ocContract x, (days (ocFrom x, ocTo x), ocFrom x, ocTo x)) | x <- occ])
                  ]
            vacancy = pdays - covered [(ocFrom x, ocTo x) | x <- occ]
            weights
              | null tenantWeights = [(E o pool, 1, "kein Mieter")]
              | ruTimeSplit r && akBasis k /= "PERSONS" && vacancy > 0 = tenantWeights <> [(E o pool, fromInteger vacancy, tshow vacancy <> " Tage Leerstand")]
              | otherwise = tenantWeights
         in splitBook "TENANT" (if akBasis k == "PERSONS" then akName k else "Tage") st acc b weights ("Objekt " <> o)
      _ -> st
    merge (n1, f1, t1) (n2, f2, t2) = (n1 + n2, min f1 f2, max t1 t2)
    -- Betrag nach Gewichten aufteilen, jeden Anteil abrunden, Rest auf R:
    splitBook stage keyName st0' from amount ws label =
      let total = sum [w | (_, w, _) <- ws]
          st = st0' {stShares = [Share to from w total keyName stage | (to, w, _) <- ws, w > 0] <> stShares st0'}
       in if total <= 0
            then note Info (ruleName r) ("Schlüsselsumme 0 – " <> accText from <> " " <> scaledText scale amount <> " bleibt offen") st0'
            else
              let shares = [(to, floorR (fromInteger amount * w / total), w, t) | (to, w, t) <- ws, w > 0]
                  rest = amount - sum [s | (_, s, _, _) <- shares]
                  st' =
                    foldl'
                      ( \s (to, amt, w, t) ->
                          book (line r to from amt (scaledText scale amount <> " × " <> rationalText 4 w <> " / " <> rationalText 4 total <> " " <> label <> " – " <> t) "" cat) s
                      )
                      st
                      shares
               in book (line r R from rest ("Rundungsrest " <> accText from) "" cat) st'

-- | Personentage je Vertrag im Zeitraum, nur während der Nutzung (optional
-- eines Objekts).
personDays :: OpInput -> Maybe Text -> Map Text Integer
personDays OpInput {..} obj =
  Map.fromListWith (+)
    [ (ocContract o, psPersons p * days iv)
    | o <- opOccupancy
    , maybe True (== ocObject o) obj
    , p <- opPersons
    , psContract p == ocContract o
    , Just iv <- [overlap (ocFrom o, ocTo o) (psFrom p, psTo p)]
    ]

-- | Mieter und Eigentümer auf Cent abrunden; Rundung nach Regel verteilen.
roundToCents :: Def -> Integer -> [Text] -> St -> (St, Integer)
roundToCents def step tenants st0 = (st2, rc)
  where
    scale = dfScale def
    mk dr cr amt txt = JLine step 0 dr cr amt txt "" "Rundung"
    final = \case
      M _ _ -> True
      T {} -> True
      E _ _ -> True
      _ -> False
    st1 = foldl' (\s (a, b) -> book (mk R a (b - (b `div` scale) * scale) "Abrunden auf Cent") s) st0 (accounts final st0)
    rc = balance R st1 `div` scale
    n = toInteger (length tenants)
    owner = E (dfUnit def) "RUNDUNG"
    st2
      | rc <= 0 = st1
      | dfRounding def == "TENANTS_OR_OWNER" && n > 0 && rc >= n =
          let q = rc `div` n
              each = foldl' (\s c -> book (mk (M c "RUNDUNG") R (q * scale) (tshow rc <> " Cent / " <> tshow n <> " Mieter = " <> tshow q <> " Cent")) s) st1 tenants
           in book (mk owner R ((rc - q * n) * scale) "Rest der Rundung an den Eigentümer") each
      | otherwise = book (mk owner R (rc * scale) (tshow rc <> " Cent Rundung an den Eigentümer")) st1

roundingText :: Def -> Int -> Integer -> Text
roundingText def n rc
  | rc == 0 = "keine Rundung"
  | dfRounding def == "TENANTS_OR_OWNER" && n > 0 && rc >= toInteger n = tshow rc <> " Cent auf " <> tshow n <> " Mieter verteilt, Rest an den Eigentümer"
  | otherwise = tshow rc <> " Cent an den Eigentümer"

-- | Prüfungen der Vollständigkeit (Fehler sperren die Freigabe, wenn das
-- Regelwerk es nicht anders erlaubt).
coverageChecks :: [Coverage] -> [Check]
coverageChecks = concatMap one
  where
    one Coverage {..} =
      let name = "Regel " <> tshow cvRule <> " Vertrag " <> cvContract
          gaps = if cvGaps && not (null cvCovered) then uncovered cvRequired cvCovered else []
       in [Check name Nothing Nothing Failure ("Abrechnung " <> u <> " erfasst, aber nicht gebucht") | u <- cvUnposted]
            <> [Check name Nothing Nothing Failure "im Vorjahr abgerechnet – für den Zeitraum fehlt die Abrechnung" | null cvCovered, cvPrevious]
            <> [Check name Nothing Nothing Failure ("nicht abgerechnet: " <> formatDay a <> "–" <> formatDay b) | (a, b) <- gaps]
            <> [Check name Nothing Nothing Ok "Zeitraum vollständig abgerechnet" | not (null cvCovered), null gaps, null cvUnposted]

-- | Teile von [a, b], die keiner der Zeiträume abdeckt.
uncovered :: (Day, Day) -> [(Day, Day)] -> [(Day, Day)]
uncovered (a, b) ivs = go a (sortOn fst [iv | Just iv <- map (overlap (a, b)) ivs])
  where
    go s [] = [(s, b) | s <= b]
    go s ((c, d) : xs)
      | c > s = (s, addDays (-1) c) : go (max s (addDays 1 d)) xs
      | otherwise = go (max s (addDays 1 d)) xs

-- --- Hilfen -----------------------------------------------------------------------

days :: (Day, Day) -> Integer
days (a, b) = diffDays b a + 1

overlap :: (Day, Day) -> (Day, Day) -> Maybe (Day, Day)
overlap (a1, b1) (a2, b2) =
  let a = max a1 a2
      b = min b1 b2
   in if a <= b then Just (a, b) else Nothing

-- | Tage, die die Zeiträume zusammen abdecken.
covered :: [(Day, Day)] -> Integer
covered = go Nothing . sortOn fst
  where
    go acc [] = maybe 0 days acc
    go Nothing (x : xs) = go (Just x) xs
    go (Just (a, b)) ((c, d) : xs)
      | c <= succ' b = go (Just (a, max b d)) xs
      | otherwise = days (a, b) + go (Just (c, d)) xs
    succ' d = toEnum (fromEnum d + 1)

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = foldr1 (\a b -> if f a >= f b then a else b)

floorR :: Rational -> Integer
floorR = floor

-- | Betrag in Cent × Faktor als Euro-Text – Cent-genau mit zwei Stellen,
-- sonst mit allen Stellen des Faktors.
scaledText :: Integer -> Integer -> Text
scaledText scale n
  | scale <= 0 = tshow n
  | n `mod` scale == 0 = formatAmount 2 (n `div` scale)
  | otherwise =
      let k = length (show scale) - 1
          dg = 2 + k + (if 10 ^ k == scale then 0 else 1)
       in formatAmount dg (roundDiv (n * 10 ^ dg) (scale * 100))

-- | Bruch mit höchstens d Nachkommastellen, ohne Nullen am Ende.
rationalText :: Int -> Rational -> Text
rationalText d x =
  let t = formatAmount d (roundDiv (numerator x * 10 ^ d) (denominator x))
   in if T.any (== '.') t then T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') t) else t

tshow :: Show a => a -> Text
tshow = T.pack . show
