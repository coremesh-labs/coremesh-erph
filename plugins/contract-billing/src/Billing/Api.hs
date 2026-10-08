-- | Schnittstelle des Plugins – ohne eigene Daten, ohne SQL.
--
-- | Object.Action | Payload | Ergebnis |
-- |---|---|---|
-- | ContractBilling.run | company_code, to_date, contract_id?, simulate?, run_id? | Belege, Meldungen (simulate: geplante Belege) |
-- | ContractBilling.loanSchedule | company_code, contract_id | Tilgungsplan (message, table) |
-- | ContractBilling.refresh | – | Cache des Mandanten verwerfen |
-- | ContractBilling.cache | – | Zustand des Caches (Diagnose) |
-- | ContractBillingCache.onEvent | SystemEvent | betroffenen Ausschnitt verwerfen |
--
-- Läufe und Sollstellungen hält das Vertragsmodul (ContractPostingRun,
-- ContractPosting); es ruft run auf. Payloads dürfen die Felder auch unter
-- "data" tragen (Oberfläche).
module Billing.Api
  ( billingPlugin
  ) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Text.Read (readMaybe)

import Billing.Cache
import Billing.Data
import Billing.Run
import CoreMesh.Plugin

callbackObject :: Text
callbackObject = "ContractBillingCache"

billingPlugin :: Cache -> Plugin
billingPlugin cache =
  (defaultPlugin "contract-billing" "0.5.0")
    { pluginDescription = "Vertragsbuchung: Sollstellung der Verträge über die Vorerfassung des Hauptbuchs"
    , pluginCapabilities =
        [ Capability "ContractBilling" ["run", "loanSchedule", "refresh", "cache"] [] "Sollstellung der Verträge rechnen und buchen, Tilgungspläne"
        , Capability callbackObject ["onEvent"] [] "Änderungen an Stammdaten (SystemEvent)"
        ]
    , pluginConfigure = \call settings -> do
        forM_ (KM.lookup "cache_ttl" settings >>= duration) (setTtl cache)
        forM_ watched $ \o -> subscribe call o "*" "*" callbackObject
    , pluginHandle = handler cache
    }

-- | "90s", "30m", "1h".
duration :: Value -> Maybe NominalDiffTime
duration = \case
  String s -> case T.unsnoc (T.strip s) of
    Just (n, u) | Just x <- (readMaybe (T.unpack n) :: Maybe Double) ->
      (\unit -> unit * realToFrac x) <$> lookup u [('s', 1), ('m', 60), ('h', 3600)]
    _ -> Nothing
  Number n -> Just (realToFrac n)
  _ -> Nothing

-- | Felder aus Payload und "data".
params :: Value -> Record
params = \case
  Object o -> case KM.lookup "data" o of
    Just (Object d) -> d <> o
    _ -> o
  _ -> mempty

handler :: Cache -> Call -> Request -> IO Response
handler cache call req = case (reqObject req, reqAction req) of
  ("ContractBilling", "run") -> do
    cc <- required "company_code" "Buchungskreis"
    toText <- required "to_date" "Buchen bis"
    to <- maybe (pluginError InvalidArgument ("Buchen bis: Datum JJJJ-MM-TT erwartet, nicht " <> toText)) pure (iso8601ParseM (T.unpack toText))
    response
      <$> runBilling cache call
        RunRequest
          { rrCompany = cc
          , rrTo = to
          , rrContract = textMaybe "contract_id" p
          , rrSimulate = bool "simulate" p
          , rrRunId = textMaybe "run_id" p
          }
  ("ContractBilling", "loanSchedule") -> do
    cc <- required "company_code" "Buchungskreis"
    cid <- required "contract_id" "Vertrag"
    response <$> loanPlan cache call cc cid
  ("ContractBilling", "refresh") -> do
    clearTenant cache (callTenantId call)
    pure (response (object ["message" .= ("Kopie der Stammdaten verworfen; der nächste Lauf lädt neu" :: Text)]))
  ("ContractBilling", "cache") -> response <$> info cache (callTenantId call)
  (o, "onEvent") | o == callbackObject -> do
    case eventOf (reqPayload req) of
      Just ev -> invalidate cache (if T.null (evTenantId ev) then callTenantId call else evTenantId ev) (evObject ev) (evCompanyCode ev)
      Nothing -> pure ()
    pure (response Null)
  (o, a) -> pluginError Unimplemented (o <> "." <> a)
  where
    p = params (reqPayload req)
    required k label = maybe (pluginError InvalidArgument (label <> " ist Pflicht")) pure (textMaybe k p)
