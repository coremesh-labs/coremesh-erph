-- | Rundung und nachgelagerte Konditionen (seit 0.13.0) – wie pkg/pricing der
-- ERP-Module (Go):
--
--   * Rundungsregel (Plugin ledger, Object RoundingRule): Art (HALF_UP
--     kaufmännisch, HALF_EVEN Bankrundung, UP, DOWN) und Schrittweite in der
--     Währung (0.01; 0.05 = Rappenrundung). Regel der Kondition, sonst der
--     Konditionsart, sonst des Buchungskreises, sonst die Standardregel, sonst
--     kaufmännisch auf die kleinste Einheit.
--   * Nachgelagerte Konditionsart (ContractDependentType): Steuer (Satz des
--     Steuerkennzeichens; ohne festes Kennzeichen das der Konditionsart der
--     Basis) oder Prozent; Zuschlag (kommt hinzu) oder Anteil (wird aus der Basis
--     herausgerechnet); Basis = Konditionsarten (leer = alle außer Steuer).
module Billing.Pricing
  ( Rounding (..)
  , Dependent (..)
  , CondType (..)
  , TaxCode (..)
  , builtinRounding
  , roundMinor
  , depRounding
  , parseDecimal
  , rounding
  , dependentType
  , condType
  , taxCode
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T

import Billing.Data (Record, bool, int, text, textMaybe)
import Billing.Depreciation (DepTerms (..))
import Billing.Schedule (roundBy)

data Rounding = Rounding
  { rCode :: Text
  , rMode :: Text -- ^ HALF_UP | HALF_EVEN | UP | DOWN
  , rIncrement :: Rational -- ^ Schrittweite in der Währung, z. B. 1 % 100
  , rDefault :: Bool -- ^ Standardregel
  , rActive :: Bool
  }
  deriving stock (Show, Eq)

-- | Kaufmännisch auf die kleinste Einheit der Währung.
builtinRounding :: Int -> Rounding
builtinRounding dec = Rounding "" "HALF_UP" (1 % (10 ^ dec)) False True

-- | Betrag in der Währung (z. B. 100.005) → gerundet in der kleinsten Einheit.
roundMinor :: Rounding -> Int -> Rational -> Integer
roundMinor r dec v =
  let inc = if rIncrement r > 0 then rIncrement r else 1 % 100
      n = roundBy (rMode r) (v / inc)
   in roundBy "HALF_UP" (fromInteger n * inc * fromInteger (10 ^ dec))

-- | Rundungsregel einer Abschreibung: die eigene, sonst die der Konditionsart
-- (z. B. AF); Schrittweite in der kleinsten Einheit. Ohne Regel bleibt es beim
-- Runden auf die kleinste Einheit.
depRounding :: Map Text Rounding -> Map Text CondType -> Int -> DepTerms -> DepTerms
depRounding roundings types dec d =
  case [r | Just code <- [dpRoundingCode d, Map.lookup (dpType d) types >>= ktRounding], Just r <- [Map.lookup code roundings], rActive r] of
    r : _ -> d {dpRound = Just (rMode r, rIncrement r * fromInteger (10 ^ dec))}
    [] -> d

-- | "1.250,5" (deutsch), "1250.5", "19" → Dezimalzahl.
parseDecimal :: Text -> Maybe Rational
parseDecimal t0 =
  let t1 = T.filter (/= ' ') (T.strip t0)
      t = if T.any (== ',') t1 then T.replace "," "." (T.filter (/= '.') t1) else t1
      (sign, body) = case T.uncons t of
        Just ('-', b) -> (-1, b)
        _ -> (1, t)
      (w, f) = T.breakOn "." body
      frac = T.drop 1 f
      digits s = not (T.null s) && T.all (`elem` ['0' .. '9']) s
   in if digits w && (T.null frac || digits frac) && (T.null f || not (T.null frac))
        then
          let whole = read (T.unpack w) :: Integer
              fr = if T.null frac then 0 else read (T.unpack frac) % (10 ^ T.length frac)
           in Just (sign * (fromInteger whole + fr))
        else Nothing

-- | Rundungsregel aus einem Datensatz des Hauptbuchs.
rounding :: Record -> Maybe Rounding
rounding r = do
  inc <- parseDecimal (text "increment" r)
  pure (Rounding (text "code" r) (text "mode" r) inc (bool "is_default" r) (activeOf r))

activeOf :: Record -> Bool
activeOf r = text "is_active" r == "" || bool "is_active" r

data Dependent = Dependent
  { dCode :: Text
  , dName :: Text
  , dMethod :: Text -- ^ TAX | PERCENT
  , dTaxCode :: Maybe Text
  , dPercent :: Maybe Rational
  , dEffect :: Text -- ^ SURCHARGE | SHARE
  , dBase :: [Text] -- ^ leer = alle außer Steuer
  , dAccount :: Maybe Text
  , dRounding :: Maybe Text
  , dSort :: Integer
  }
  deriving stock (Show, Eq)

dependentType :: Record -> Dependent
dependentType r =
  Dependent
    { dCode = text "code" r
    , dName = text "name" r
    , dMethod = text "method" r
    , dTaxCode = textMaybe "tax_code" r
    , dPercent = textMaybe "percent" r >>= parseDecimal
    , dEffect = maybe "SURCHARGE" id (textMaybe "effect" r)
    , dBase = filter (not . T.null) (map (T.toUpper . T.strip) (T.splitOn "," (text "base_types" r)))
    , dAccount = textMaybe "account_number" r
    , dRounding = textMaybe "rounding_rule" r
    , dSort = maybe 0 id (int "sort_order" r)
    }

-- | Konditionsart: Steuerkennzeichen und Rundungsregel.
data CondType = CondType
  { ktCode :: Text
  , ktTaxCode :: Maybe Text
  , ktRounding :: Maybe Text
  }
  deriving stock (Show, Eq)

condType :: Record -> CondType
condType r = CondType (text "code" r) (textMaybe "tax_code" r) (textMaybe "rounding_rule" r)

data TaxCode = TaxCode
  { tcCode :: Text
  , tcRate :: Rational -- ^ Prozent
  , tcAccount :: Maybe Text
  }
  deriving stock (Show, Eq)

taxCode :: Record -> Maybe TaxCode
taxCode r = do
  rate <- parseDecimal (text "rate" r)
  pure (TaxCode (text "code" r) rate (textMaybe "account_number" r))
