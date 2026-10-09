-- | Kopie der Stammdaten im Speicher – das Plugin hat keine eigenen Tabellen.
--
--   * Geladen wird je Mandant, Buchungskreis und Ausschnitt (Verträge,
--     Konditionen …) als Datenstrom über eine Systemanfrage: unabhängig von
--     den Rechten des Benutzers, der gerade bucht (die prüft "Billing.Run").
--   * Ein SystemEvent des Objects macht den Ausschnitt ungültig; geladen wird
--     er erst beim nächsten Gebrauch neu (eine Folge von Änderungen = ein
--     Ladevorgang).
--   * Sicherheitsnetz: Events gehen höchstens einmal zu – nach der Haltezeit
--     (Setting cache_ttl, Standard 1 h) wird jeder Ausschnitt neu geladen.
module Billing.Cache
  ( Cache
  , Slice (..)
  , newCache
  , setTtl
  , slice
  , invalidate
  , clearTenant
  , watched
  , info
  , readAll
  , readAllOrEmpty
  ) where

import Control.Exception (try)
import Control.Concurrent.STM
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import Billing.Data (Record, records)
import CoreMesh.Plugin

-- | Ausschnitte der Stammdaten.
data Slice = Types | Contracts | Conditions | Objects | Measurements | Accounts | FieldStatuses | Loans | LoanPayments | Depreciations
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Quelle: Object, Filterfeld des Buchungskreises (Nothing = gilt für alle), mit Historie.
source :: Slice -> (Text, Maybe Text, Bool)
source = \case
  Types -> ("ContractType", Just "company_code", False)
  Contracts -> ("Contract", Just "company_code", True)
  Conditions -> ("ContractCondition", Just "company_code", True)
  Objects -> ("ContractObject", Just "company_code", True)
  Measurements -> ("Measurement", Just "company_code", True)
  Accounts -> ("GLAccountCompany", Just "company_code_id", False)
  FieldStatuses -> ("FieldStatus", Nothing, False)
  Loans -> ("ContractLoan", Just "company_code", True)
  LoanPayments -> ("ContractLoanPayment", Just "company_code", False)
  Depreciations -> ("ContractDepreciation", Just "company_code", True)

-- | Objects, deren Events den Cache betreffen (für SystemEvent.Register).
watched :: [Text]
watched = [o | s <- [minBound .. maxBound], let (o, _, _) = source s]

data Entry = Entry
  { enRecords :: [Record]
  , enLoaded :: UTCTime
  }

type Key = (Text, Text, Slice) -- Mandant, Buchungskreis ("*" = alle), Ausschnitt

data Cache = Cache
  { cEntries :: TVar (Map Key Entry)
  , cTtl :: IORef NominalDiffTime
  , cLoads :: IORef Int
  }

newCache :: IO Cache
newCache = Cache <$> newTVarIO mempty <*> newIORef 3600 <*> newIORef 0

setTtl :: Cache -> NominalDiffTime -> IO ()
setTtl c = writeIORef (cTtl c)

keyOf :: Text -> Text -> Slice -> Key
keyOf tenant cc s = case source s of
  (_, Nothing, _) -> (tenant, "*", s)
  _ -> (tenant, cc, s)

-- | Datensätze eines Ausschnitts; lädt nach, wenn er fehlt, ungültig oder zu alt ist.
-- sys ist eine Systemanfrage im Mandanten (systemCall).
slice :: Cache -> Call -> Text -> Slice -> IO [Record]
slice c sys cc s = do
  let key = keyOf (callTenantId sys) cc s
  now <- getCurrentTime
  ttl <- readIORef (cTtl c)
  cached <- Map.lookup key <$> readTVarIO (cEntries c)
  case cached of
    Just e | diffUTCTime now (enLoaded e) < ttl -> pure (enRecords e)
    _ -> do
      let (obj, ccField, history) = source s
          params = [(f, String cc) | Just f <- [ccField]] <> [("includeHistory", Bool True) | history]
      rs <- readAll sys obj params
      atomically $ modifyTVar' (cEntries c) (Map.insert key (Entry rs now))
      modifyIORef' (cLoads c) (+ 1)
      pure rs

-- | Event zu einem Object: betroffene Ausschnitte des Buchungskreises (leer =
-- aller Buchungskreise) im Mandanten verwerfen.
invalidate :: Cache -> Text -> Text -> Text -> IO ()
invalidate c tenant obj cc =
  atomically . modifyTVar' (cEntries c) . Map.filterWithKey $ \(t, k, s) _ ->
    let (o, _, _) = source s
     in not (t == tenant && o == obj && (cc == "" || k == cc || k == "*"))

clearTenant :: Cache -> Text -> IO ()
clearTenant c tenant = atomically . modifyTVar' (cEntries c) $ Map.filterWithKey (\(t, _, _) _ -> t /= tenant)

-- | Zustand für ContractBilling.cache (Diagnose).
info :: Cache -> Text -> IO Value
info c tenant = do
  now <- getCurrentTime
  es <- readTVarIO (cEntries c)
  loads <- readIORef (cLoads c)
  ttl <- readIORef (cTtl c)
  pure $
    object
      [ "loads" .= loads
      , "ttl_seconds" .= (realToFrac ttl :: Double)
      , "items"
          .= [ object
                 [ "company_code" .= cc
                 , "slice" .= show s
                 , "records" .= length (enRecords e)
                 , "age_seconds" .= (realToFrac (diffUTCTime now (enLoaded e)) :: Double)
                 ]
             | ((t, cc, s), e) <- Map.toList es
             , t == tenant
             ]
      ]

-- | Liest (object, list) als Datenstrom vollständig.
-- | Wie readAll; ist das Object nicht erreichbar (Modul fehlt), leer.
readAllOrEmpty :: Call -> Text -> [(Text, Value)] -> IO [Record]
readAllOrEmpty call obj params = either (\(_ :: PluginError) -> []) id <$> try (readAll call obj params)

readAll :: Call -> Text -> [(Text, Value)] -> IO [Record]
readAll call obj params = do
  cols <- newIORef []
  rows <- newIORef []
  _ <-
    dispatchRead call obj "list" (Object (KM.fromList [(K.fromText k, v) | (k, v) <- params])) $
      RowWriter
        { writeHeader = writeIORef cols . headerColumns
        , writeRows = \rs -> modifyIORef' rows (reverse rs <>)
        }
  records <$> readIORef cols <*> (reverse <$> readIORef rows)
