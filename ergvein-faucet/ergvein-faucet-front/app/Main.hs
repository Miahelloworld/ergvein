module Main where

import Ergvein.Faucet.Frontend
import Ergvein.Faucet.Frontend.Style
import Reflex.Dom

main :: IO ()
main = mainWidgetWithCss frontendCssBS frontend
