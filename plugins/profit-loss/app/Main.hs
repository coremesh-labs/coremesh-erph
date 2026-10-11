-- | Plugin profit-loss: GuV-Rechnung (siehe "ProfitLoss.Api").
module Main (main) where

import CoreMesh.Plugin (serve)
import Data.IORef (newIORef)
import ProfitLoss.Api (profitLossPlugin)
import ProfitLoss.Report (defaultSettings)

main :: IO ()
main = newIORef defaultSettings >>= serve . profitLossPlugin
