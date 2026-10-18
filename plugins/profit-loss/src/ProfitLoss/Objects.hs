-- | Zuordnung eines Einzelpostens zu Gebäude und Wirtschaftseinheit – ohne
-- Ein-/Ausgabe.
--
-- Die Buchungen tragen das Objekt auf verschiedenen Wegen: Eingangsrechnungen
-- das Gebäude in einer Kontierungsspalte (Einstellung building_field, z. B.
-- dimension_custom_1), Sollstellungen der Mieten das Mietobjekt, Zinsen und
-- Hausgeld nur den Vertrag. Gesucht wird in dieser Reihenfolge:
--
--   1. Kontierungsspalte des Gebäudes (ist dort ein Mietobjekt eingetragen,
--      gilt dessen Gebäude),
--   2. Mietobjekt → sein Gebäude,
--   3. Vertrag → sein Objekt am Buchungsdatum (Hauptobjekt zuerst):
--      Gebäude direkt, Mietobjekt → sein Gebäude.
--
-- Ohne Treffer bleibt der Posten ohne Gebäude („nicht zugeordnet“).
module ProfitLoss.Objects
  ( Objects (..)
  , ContractObject (..)
  , noObjects
  , buildingOf
  , entityOf
  , entityOfPosting
  , buildingName
  , entityName
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)

-- | Objekt eines Vertrags mit Gültigkeit.
data ContractObject = ContractObject
  { coType :: Text
  -- ^ Building, RentObject, …
  , coId :: Text
  , coMain :: Bool
  , coFrom :: Maybe Day
  , coTo :: Maybe Day
  }
  deriving stock (Eq, Show)

data Objects = Objects
  { obBuildings :: Map.Map Text (Text, Text)
  -- ^ Gebäude → (Bezeichnung, Wirtschaftseinheit)
  , obRentObjects :: Map.Map Text Text
  -- ^ Mietobjekt → Gebäude
  , obContracts :: Map.Map Text [ContractObject]
  -- ^ Vertrag → Objekte
  , obEntities :: Map.Map Text Text
  -- ^ Wirtschaftseinheit → Bezeichnung
  }
  deriving stock (Eq, Show)

noObjects :: Objects
noObjects = Objects mempty mempty mempty mempty

-- | Gebäude eines Postens: Wert der Gebäudespalte, Mietobjekt, Vertrag, Buchungsdatum.
buildingOf :: Objects -> Text -> Text -> Text -> Day -> Text
buildingOf Objects {..} dim rentObject contract date
  | not (T.null dim) = Map.findWithDefault dim dim obRentObjects
  | Just b <- Map.lookup rentObject obRentObjects = b
  | otherwise = maybe "" fromContract (Map.lookup contract obContracts)
  where
    fromContract objs =
      case [b | o <- sortOn (Down . coMain) objs, valid o, Just b <- [building o]] of
        b : _ -> b
        [] -> ""
    valid o = maybe True (<= date) (coFrom o) && maybe True (>= date) (coTo o)
    building o = case coType o of
      "Building" -> Just (coId o)
      "RentObject" -> Map.lookup (coId o) obRentObjects
      _ -> Nothing

-- | Wirtschaftseinheit eines Gebäudes ("" ohne).
entityOf :: Objects -> Text -> Text
entityOf Objects {..} b = maybe "" snd (Map.lookup b obBuildings)

-- | Wirtschaftseinheit eines Postens: über sein Gebäude, sonst ein Vertrag direkt
-- an der Wirtschaftseinheit (Objekt BusinessEntity).
entityOfPosting :: Objects -> Text -> Text -> Day -> Text
entityOfPosting o@Objects {..} building contract date
  | not (T.null building) = entityOf o building
  | otherwise =
      case [coId c | c <- sortOn (Down . coMain) (Map.findWithDefault [] contract obContracts), coType c == "BusinessEntity", valid c] of
        e : _ -> e
        [] -> ""
  where
    valid c = maybe True (<= date) (coFrom c) && maybe True (>= date) (coTo c)

-- | „Bezeichnung (Nummer)“ eines Gebäudes; ohne Gebäude „nicht zugeordnet“.
buildingName :: Objects -> Text -> Text
buildingName Objects {..} b
  | T.null b = "nicht zugeordnet"
  | Just (n, _) <- Map.lookup b obBuildings, not (T.null n) = n <> " (" <> b <> ")"
  | otherwise = b

entityName :: Objects -> Text -> Text
entityName Objects {..} e
  | T.null e = "nicht zugeordnet"
  | Just n <- Map.lookup e obEntities, not (T.null n) = n <> " (" <> e <> ")"
  | otherwise = e
