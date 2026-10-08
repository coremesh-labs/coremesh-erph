-- | Plugin contract-billing: Sollstellung der Verträge (siehe "Billing.Api").
module Main (main) where

import Billing.Api (billingPlugin)
import Billing.Cache (newCache)
import CoreMesh.Plugin (serve)

main :: IO ()
main = newCache >>= serve . billingPlugin
