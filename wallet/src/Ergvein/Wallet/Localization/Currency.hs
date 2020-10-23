{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localization.Currency(
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

instance LocalizedPrint Coin where
  localizedShow _ v = case v of
    BTC   -> "BTC"
    TBTC  -> "Testnet BTC"
    RTBTC -> "Regtest BTC"
    ERGO  -> "ERGO"
    TERGO -> "Testnet ERGO"

instance LocalizedPrint Currency where
  localizedShow _ v = case v of
    Bitcoin -> "Bitcoin"
    Ergo    -> "Ergo"
