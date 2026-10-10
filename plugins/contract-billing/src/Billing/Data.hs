-- | Daten aus den anderen Plugins, gelesen als Datenstrom (Rohwerte der
-- Tabellen, siehe crud ReadList): Verträge, Konditionen und Objekte (contract),
-- Bemessungen (realestate), Konten und Feldstatus (ledger), Buchungskreisdaten
-- der Partner (partner).
module Billing.Data
  ( Record
  , records
  , ContractType (..)
  , Contract (..)
  , Condition (..)
  , ContractObject (..)
  , Measurement (..)
  , Account (..)
  , Posted (..)
  , PartnerAccount (..)
  , PostingKind (..)
  , FieldStatus
  , contractType
  , contract
  , condition
  , contractObject
  , measurement
  , account
  , posted
  , partnerAccount
  , loanTerms
  , loanPayment
  , depTerms
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

import Billing.Depreciation
import Billing.Loan
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
  , ctRole :: Text -- ^ Rolle des Vertragspartners; ihre Buchungskreisdaten tragen das Abstimmkonto
  , ctDocumentType :: Maybe Text -- ^ Belegart der Sollstellung
  , ctCreditDocumentType :: Maybe Text -- ^ Belegart, wenn der Beleg per Saldo eine Gutschrift ist
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
  , cBillingStart :: Maybe Day -- ^ Sollstellung ab (z. B. Übernahme aus einem Altsystem); leer = Beginn
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
  , kStartMonth :: Int -- ^ Periodenbeginn (1 = Kalender), z. B. 10 = Okt–Sep
  , kDueMonth :: Int -- ^ vorschüssig fällig im n-ten Monat der Periode (1 = erster)
  , kDueDate :: Maybe Day -- ^ nur einmalig: Fälligkeit (leer = Beginn)
  , kInstallments :: Int -- ^ nur einmalig: Monatsraten (Standard 1)
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

-- | Buchungskreisdaten eines Partners in einer Finanzrolle (PartnerCompanyCode).
data PartnerAccount = PartnerAccount
  { paPartner :: Text
  , paRole :: Text
  , paAccount :: Maybe Text -- ^ Abstimmkonto
  , paPostingBlock :: Bool
  }
  deriving stock (Show)

partnerAccount :: Record -> PartnerAccount
partnerAccount r = PartnerAccount (text "bp_id" r) (text "role_code" r) (textMaybe "reconciliation_account" r) (bool "posting_block" r)

-- | Erste Sollstellung zur Fälligkeit oder Nachberechnung zum Lauf.
data PostingKind = Original | Correction
  deriving stock (Eq, Show)

-- | Ein vermerkter Teil einer Sollstellung (ContractPosting).
data Posted = Posted
  { pContract :: Text
  , pType :: Text
  , pObject :: Text
  , pFrom :: Day -- ^ abgedeckt von (Nachberechnung: Beginn der Periode)
  , pTo :: Day
  , pPeriod :: Maybe Day -- ^ Beginn der Kalenderperiode; leer bei Vermerken vor 0.4.0
  , pDue :: Day
  , pAmount :: Integer
  , pKind :: PostingKind
  , pAccount :: Text
  , pRentObject :: Maybe Text
  }
  deriving stock (Show)

posted :: Record -> Maybe Posted
posted r = do
  from <- day "period_from" r
  to <- day "period_to" r
  due <- day "due_date" r
  amount <- int "amount" r
  pure
    Posted
      { pContract = text "contract_id" r
      , pType = text "condition_type" r
      , pObject = text "object_id" r
      , pFrom = from
      , pTo = to
      , pPeriod = day "billing_period" r
      , pDue = due
      , pAmount = amount
      , pKind = if text "kind" r == "CORRECTION" then Correction else Original
      , pAccount = text "account_number" r
      , pRentObject = textMaybe "rent_object_id" r
      }

-- | Feldstatus: (Gruppe, Feld) → SUPPRESS | OPTIONAL | REQUIRED; fehlt = ausgeblendet.
type FieldStatus = Map (Text, Text) Text

until' :: Text -> Record -> Day
until' k r = maybe openEnd id (day k r)

contractType :: Record -> ContractType
contractType r =
  ContractType
    { ctCode = text "code" r
    , ctDirection = text "direction" r
    , ctRole = text "main_role" r
    , ctDocumentType = textMaybe "posting_document_type" r
    , ctCreditDocumentType = textMaybe "credit_document_type" r
    , ctAutoPost = bool "auto_post" r
    }

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
      , cBillingStart = day "billing_start" r
      }

condition :: Record -> Either Text Condition
condition r = do
  from <- maybe (Left "Beginn fehlt") Right (day "valid_from" r)
  amount <- maybe (Left "Betrag fehlt") Right (int "amount" r)
  freq <- case text "frequency" r of
    "MONTHLY" -> Right Monthly
    "BIMONTHLY" -> Right Bimonthly
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
      , kStartMonth = maybe 1 fromInteger (int "period_start_month" r)
      , kDueMonth = maybe 1 fromInteger (int "due_month" r)
      , kDueDate = day "due_date" r
      , kInstallments = maybe 1 fromInteger (int "installments" r)
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
account r = Account (text "account_number" r) (text "field_status_group" r) (bool "is_blocked" r)

fieldStatus :: [Record] -> FieldStatus
fieldStatus rs = Map.fromList [((text "group_id" r, text "field_name" r), text "status" r) | r <- rs]

-- | Darlehenskonditionen (ContractLoan, eine Zeitscheibe).
loanTerms :: Record -> Either Text LoanTerms
loanTerms r = do
  from <- maybe (Left "Gültig ab fehlt") Right (day "valid_from" r)
  start <- maybe (Left "Auszahlung fehlt") Right (day "disbursement_date" r)
  principal <- maybe (Left "Darlehensbetrag fehlt") Right (int "principal" r)
  rate <- maybe (Left ("Zinssatz " <> text "interest_rate" r)) Right (parseRate (text "interest_rate" r))
  repayment <- case text "repayment_type" r of
    "ANNUITY" -> Right Annuity
    "INSTALLMENT" -> Right Instalment
    "BULLET" -> Right Bullet
    t -> Left ("unbekannte Tilgungsart " <> t)
  freq <- case text "frequency" r of
    "MONTHLY" -> Right Monthly
    "BIMONTHLY" -> Right Bimonthly
    "QUARTERLY" -> Right Quarterly
    "HALF_YEARLY" -> Right HalfYearly
    "YEARLY" -> Right Yearly
    f -> Left ("unbekannter Rhythmus " <> f)
  dc <- case text "day_count" r of
    "30/360" -> Right D30360
    "ACT/360" -> Right ACT360
    "ACT/365" -> Right ACT365
    d -> Left ("unbekannte Zinsmethode " <> d)
  pure
    LoanTerms
      { lnContract = text "contract_id" r
      , lnFrom = from
      , lnTo = until' "valid_to" r
      , lnPrincipal = principal
      , lnStart = start
      , lnTakeover = bool "takeover" r
      , lnRepayment = repayment
      , lnRate = rate
      , lnInstallment = maybe 0 id (int "installment" r)
      , lnFrequency = freq
      , lnDueDay = maybe 30 fromInteger (int "due_day" r)
      , lnDayCount = dc
      , lnLoanAccount = text "loan_account" r
      , lnInterestAccount = text "interest_account" r
      , lnInterestType = text "interest_type" r
      , lnPrincipalType = text "principal_type" r
      , lnSpecialType = text "special_type" r
      , lnDisbursementType = text "disbursement_type" r
      }

-- | Abschreibungskondition (ContractDepreciation).
depTerms :: Record -> Either Text DepTerms
depTerms r = do
  from <- maybe (Left "Gültig ab fehlt") Right (day "valid_from" r)
  base <- maybe (Left "Bemessungsgrundlage fehlt") Right (int "base_amount" r)
  rate <- maybe (Left ("Satz " <> text "rate" r)) Right (parseRate (text "rate" r))
  method <- case text "method" r of
    "LINEAR" -> Right Linear
    "DEGRESSIVE" -> Right Degressive
    m -> Left ("unbekannte Abschreibungsmethode " <> m)
  pure
    DepTerms
      { dpContract = text "contract_id" r
      , dpFrom = from
      , dpTo = until' "valid_to" r
      , dpBase = base
      , dpRate = rate
      , dpMethod = method
      , dpStart = maybe from id (day "start_date" r)
      , dpOpening = int "opening_value" r
      , dpAssetAccount = text "asset_account" r
      , dpExpenseAccount = text "expense_account" r
      , dpType = let t = text "condition_type" r in if T.null t then "AF" else t
      }

loanPayment :: Record -> Maybe LoanPayment
loanPayment r = LoanPayment (text "contract_id" r) <$> day "payment_date" r <*> int "amount" r
