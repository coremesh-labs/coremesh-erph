-- | Daten aus den anderen Plugins, gelesen als Datenstrom (Rohwerte der
-- Tabellen, siehe crud ReadList): Verträge, Konditionen und Objekte (contract),
-- Bemessungen (realestate), Konten und Feldstatus (ledger).
module Billing.Data
  ( Record
  , records
  , ContractType (..)
  , Contract (..)
  , Condition (..)
  , ContractObject (..)
  , Measurement (..)
  , Account (..)
  , FieldStatus
  , contractType
  , contract
  , condition
  , contractObject
  , measurement
  , account
  , fieldStatus
  , text
  , textMaybe
  , day
  , int
  , number
  , bool
  , openEnd
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Billing.Schedule (Frequency (..), PaymentMode (..))

-- | Eine Zeile als Spalte → Wert.
type Record = KM.KeyMap Value

records :: [Text] -> [[Value]] -> [Record]
records cols = map (KM.fromList . zip (map K.fromText cols))

text :: Text -> Record -> Text
text k r = case KM.lookup (K.fromText k) r of
  Just (String s) -> s
  Just (Number n) -> maybe (T.pack (show n)) (T.pack . show) (toBoundedInteger n :: Maybe Int)
  Just (Bool b) -> if b then "true" else "false"
  _ -> ""

textMaybe :: Text -> Record -> Maybe Text
textMaybe k r = case T.strip (text k r) of
  "" -> Nothing
  t -> Just t

-- | Unbefristet: leer oder 9999-12-31.
openEnd :: Day
openEnd = fromGregorian 9999 12 31

day :: Text -> Record -> Maybe Day
day k r = iso8601ParseM (T.unpack (T.take 10 (text k r)))

int :: Text -> Record -> Maybe Integer
int k r = case KM.lookup (K.fromText k) r of
  Just (Number n) -> toBoundedInteger n >>= Just . (toInteger :: Int -> Integer)
  Just (String s) -> case reads (T.unpack s) of
    [(n, "")] -> Just n
    _ -> Nothing
  _ -> Nothing

number :: Text -> Record -> Maybe Scientific
number k r = case KM.lookup (K.fromText k) r of
  Just (Number n) -> Just n
  Just (String s) -> case reads (T.unpack (T.replace "," "." s)) :: [(Double, String)] of
    [(x, "")] -> Just (realToFrac x)
    _ -> Nothing
  _ -> Nothing

bool :: Text -> Record -> Bool
bool k r = case KM.lookup (K.fromText k) r of
  Just (Bool b) -> b
  Just (String s) -> s `elem` ["true", "1", "TRUE"]
  Just (Number n) -> n /= 0
  _ -> False

-- | Vertragsart: Richtung und Steuerung der Sollstellung.
data ContractType = ContractType
  { ctCode :: Text
  , ctDirection :: Text -- ^ RECEIVABLE | PAYABLE
  , ctReconciliation :: Maybe Text -- ^ Abstimmkonto (Debitor bzw. Kreditor)
  , ctDocumentType :: Maybe Text -- ^ Belegart der Sollstellung
  , ctAutoPost :: Bool -- ^ geprüfte Vorerfassung gleich buchen (sonst offen lassen)
  }
  deriving stock (Show)

data Contract = Contract
  { cCompany :: Text
  , cId :: Text
  , cType :: Text
  , cStatus :: Text
  , cPartner :: Text
  , cCurrency :: Text
  , cDesignation :: Text
  , cFrom :: Day
  , cTo :: Day
  }
  deriving stock (Show)

data Condition = Condition
  { kContract :: Text
  , kType :: Text
  , kObject :: Text -- ^ leer = ganzer Vertrag
  , kPerUnit :: Bool
  , kAmount :: Integer -- ^ kleinste Währungseinheit (je Einheit bei kPerUnit)
  , kMeasurement :: Maybe Text
  , kFrequency :: Frequency
  , kDueDay :: Int
  , kMode :: PaymentMode
  , kAccount :: Maybe Text
  , kPayer :: Maybe Text
  , kFrom :: Day
  , kTo :: Day
  }
  deriving stock (Show)

data ContractObject = ContractObject
  { oContract :: Text
  , oType :: Text -- ^ RentObject | Building | BusinessEntity
  , oId :: Text
  , oMain :: Bool
  , oFrom :: Day
  , oTo :: Day
  }
  deriving stock (Show)

data Measurement = Measurement
  { mObject :: Text
  , mType :: Text
  , mValue :: Scientific
  , mFrom :: Day
  , mTo :: Day
  }
  deriving stock (Show)

-- | Sachkonto im Buchungskreis (Nummer ohne Kontenplan-Präfix).
data Account = Account
  { aNumber :: Text
  , aGroup :: Text -- ^ Feldstatusgruppe (leer = STD)
  , aBlocked :: Bool
  }
  deriving stock (Show)

-- | Feldstatus: (Gruppe, Feld) → SUPPRESS | OPTIONAL | REQUIRED; fehlt = ausgeblendet.
type FieldStatus = Map (Text, Text) Text

until' :: Text -> Record -> Day
until' k r = maybe openEnd id (day k r)

contractType :: Record -> ContractType
contractType r =
  ContractType (text "code" r) (text "direction" r) (textMaybe "reconciliation_account" r) (textMaybe "posting_document_type" r) (bool "auto_post" r)

contract :: Record -> Maybe Contract
contract r = do
  from <- day "valid_from" r
  pure
    Contract
      { cCompany = text "company_code" r
      , cId = text "contract_id" r
      , cType = text "contract_type" r
      , cStatus = text "status" r
      , cPartner = text "partner_id" r
      , cCurrency = maybe "EUR" id (textMaybe "currency" r)
      , cDesignation = text "designation" r
      , cFrom = from
      , cTo = until' "valid_to" r
      }

condition :: Record -> Either Text Condition
condition r = do
  from <- maybe (Left "Beginn fehlt") Right (day "valid_from" r)
  amount <- maybe (Left "Betrag fehlt") Right (int "amount" r)
  freq <- case text "frequency" r of
    "MONTHLY" -> Right Monthly
    "QUARTERLY" -> Right Quarterly
    "HALF_YEARLY" -> Right HalfYearly
    "YEARLY" -> Right Yearly
    "ONCE" -> Right Once
    f -> Left ("unbekannter Rhythmus " <> f)
  let mode = if text "payment_mode" r == "IN_ARREARS" then InArrears else InAdvance
  pure
    Condition
      { kContract = text "contract_id" r
      , kType = text "condition_type" r
      , kObject = text "object_id" r
      , kPerUnit = text "calc_method" r == "PER_UNIT"
      , kAmount = amount
      , kMeasurement = textMaybe "measurement_type" r
      , kFrequency = freq
      , kDueDay = maybe 1 fromInteger (int "due_day" r)
      , kMode = mode
      , kAccount = textMaybe "account_number" r
      , kPayer = textMaybe "payer_id" r
      , kFrom = from
      , kTo = until' "valid_to" r
      }

contractObject :: Record -> Maybe ContractObject
contractObject r = do
  from <- day "valid_from" r
  pure (ContractObject (text "contract_id" r) (text "object_type" r) (text "object_id" r) (bool "is_main" r) from (until' "valid_to" r))

measurement :: Record -> Maybe Measurement
measurement r = do
  from <- day "valid_from" r
  v <- number "value" r
  pure (Measurement (text "object_id" r) (text "measurement_type" r) v from (until' "valid_to" r))

account :: Record -> Account
account r = Account (shortAccount (text "account_number" r)) (text "field_status_group" r) (bool "is_blocked" r)

-- | Kontonummer ohne Kontenplan-Präfix ("SKR25-1200" → "1200").
shortAccount :: Text -> Text
shortAccount a = case T.breakOnEnd "-" a of
  ("", _) -> a
  (_, n) -> n

fieldStatus :: [Record] -> FieldStatus
fieldStatus rs = Map.fromList [((text "group_id" r, text "field_name" r), text "status" r) | r <- rs]
