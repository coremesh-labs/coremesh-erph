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
module Billing.OpCost
  ( Def (..)
  , Rule (..)
  , AllocKey (..)
  , Target (..)
  , Source (..)
  , Occupancy (..)
  , PersonSlice (..)
  , OpInput (..)
  , Acc (..)
  , JLine (..)
  , TenantResult (..)
  , OpResult (..)
  , computeOpCost
  , accText
  , scaledText
  , overlap
  , days
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, diffDays)

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
data Target = ToPool | ToObject Text | ToTenant Text
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
  , opOccupancy :: [Occupancy]
  , opPersons :: [PersonSlice]
  , opSources :: [Source]
  , opAdvances :: Map Text Integer -- ^ Vorauszahlungen je Vertrag (Cent)
  , opCategories :: Map Text Text -- ^ Kostenart → Bezeichnung (Ausweis beim Mieter)
  }

data Acc = Q Text Text | S Text | O Text Text | M Text Text | E Text Text | R
  deriving stock (Eq, Ord, Show)

accText :: Acc -> Text
accText = \case
  Q t v -> "Q:" <> t <> ":" <> v
  S p -> "S:" <> p
  O o p -> "O:" <> o <> ":" <> p
  M c p -> "M:" <> c <> ":" <> p
  E o p -> "E:" <> o <> ":" <> p
  R -> "R:"

poolOf :: Acc -> Maybe Text
poolOf = \case
  S p -> Just p
  O _ p -> Just p
  M _ p -> Just p
  E _ p -> Just p
  _ -> Nothing

withPool :: Text -> Acc -> Acc
withPool p = \case
  S _ -> S p
  O o _ -> O o p
  M c _ -> M c p
  E o _ -> E o p
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
  }

-- --- Rechenzustand ----------------------------------------------------------------

data St = St
  { stBal :: Map Acc Integer
  , stJournal :: [JLine] -- ^ umgekehrt
  , stNotes :: [Check] -- ^ umgekehrt
  }

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
    , orChecks = reverse (stNotes final) <> endChecks
    , orSources = sourcesCents
    , orTenantTotal = sum (map trCosts tenants)
    , orOwnerTotal = ownerCents
    , orRounding = roundingCents
    }
  where
    def = opDef
    scale = dfScale def
    rules = sortOn (\r -> (ruStep r, ruNo r)) opRules
    lastStep = maybe 0 ruStep (listToMaybe (reverse rules))
    afterRules = foldl' (flip (applyRule inp)) (St mempty [] []) rules
    (final, roundingCents) = roundToCents def (lastStep + 1) (tenantIds inp) afterRules
    journal = reverse (stJournal final)
    cents a = balance a final `div` scale
    sourcesCents = negate (sum [b | (Q _ _, b) <- Map.toList (stBal final)]) `div` scale
    ownerCents = sum [b `div` scale | (E _ _, b) <- Map.toList (stBal final)]
    labels = Map.map (\c -> Map.findWithDefault c c opCategories) (poolLabels rules)
    tenants =
      [ TenantResult
          { trContract = c
          , trPartner = partner
          , trObject = fst (maximumOn snd (Map.toList (Map.fromListWith (+) [(ocObject o, days (ocFrom o, ocTo o)) | o <- os])))
          , trFrom = minimum (map ocFrom os)
          , trTo = maximum (map ocTo os)
          , trCosts = sum (map snd ls)
          , trAdvances = Map.findWithDefault 0 c opAdvances
          , trLines = ls
          }
      | (c, os@(o0 : _)) <- Map.toList (Map.fromListWith (flip (<>)) [(ocContract o, [o]) | o <- opOccupancy])
      , let partner = ocPartner o0
            ls = [(Map.findWithDefault p p labels, cents a) | (a@(M c' p), _) <- Map.toList (stBal final), c' == c, cents a /= 0]
      ]
    leftover =
      [ Check ("Offen " <> accText a) Nothing (Just (b `div` scale)) Failure
          ("Kosten bleiben liegen: " <> scaledText scale b <> " – keine Verteilregel oder Schlüsselsumme 0")
      | (a, b) <- accounts isOpen final
      ]
    isOpen = \case
      S _ -> True
      O _ _ -> True
      _ -> False
    distributed = sum (map trCosts tenants) + ownerCents
    endChecks =
      leftover
        <> [ Check "Summe" (Just sourcesCents) (Just distributed) (if sourcesCents == distributed then Ok else Deviation)
               "Summe der Quellen = Mieter + Eigentümer (Leerstand, Rundung)"
           , Check "Rundung" Nothing (Just roundingCents) Info (roundingText def (length tenants) roundingCents)
           ]

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
    mine = [s | s <- opSources, soType s == ruSourceType r, soValue s == ruSourceValue r]
    pool = ruPool r
    one s =
      let to = case soTarget s of
            ToPool -> S pool
            ToObject o -> O o pool
            ToTenant c -> M c pool
       in book (line r to (Q (soType s) (soValue s)) (soAmount s * scale) (soFormula s) (soRef s) (category r (soCategory s)))

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
    "MEASUREMENT" -> toTenants k (toObjects k (measured k) st0)
    "UNITS" -> toTenants k (toObjects k [(o, 1) | o <- opObjects] st0)
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
    -- Topf → Mietobjekte nach Gewicht
    toObjects k ws st =
      let a = balance (S pool) st
       in if a == 0 then st else splitBook st (S pool) a [(O o pool, w, rationalText 4 w) | (o, w) <- ws] ("(" <> akName k <> ")")
    -- Topf → Mieter nach Personentagen
    personsDirect k st =
      let a = balance (S pool) st
          ws = [(M c pool, w, tshow w <> " Personentage") | (c, w) <- Map.toList (personDays inp Nothing)]
       in if a == 0 then st else splitBook st (S pool) a [(acc, fromInteger w, t) | (acc, w, t) <- ws] ("(" <> akName k <> ")")
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
                  [ (M c pool, fromInteger w, tshow w <> " Personentage")
                  | (c, w) <- Map.toList (personDays inp (Just o)), w > 0
                  ]
              | otherwise =
                  [ (M c pool, fromInteger n, tshow n <> " Tage " <> formatDay f <> "–" <> formatDay t)
                  | (c, (n, f, t)) <- Map.toList (Map.fromListWith merge [(ocContract x, (days (ocFrom x, ocTo x), ocFrom x, ocTo x)) | x <- occ])
                  ]
            vacancy = pdays - covered [(ocFrom x, ocTo x) | x <- occ]
            weights
              | null tenantWeights = [(E o pool, 1, "kein Mieter")]
              | ruTimeSplit r && akBasis k /= "PERSONS" && vacancy > 0 = tenantWeights <> [(E o pool, fromInteger vacancy, tshow vacancy <> " Tage Leerstand")]
              | otherwise = tenantWeights
         in splitBook st acc b weights ("Objekt " <> o)
      _ -> st
    merge (n1, f1, t1) (n2, f2, t2) = (n1 + n2, min f1 f2, max t1 t2)
    -- Betrag nach Gewichten aufteilen, jeden Anteil abrunden, Rest auf R:
    splitBook st from amount ws label =
      let total = sum [w | (_, w, _) <- ws]
       in if total <= 0
            then note Info (ruleName r) ("Schlüsselsumme 0 – " <> accText from <> " " <> scaledText scale amount <> " bleibt offen") st
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
