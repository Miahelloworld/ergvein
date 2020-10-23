{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Reflex.Dom.Main (mainWidgetWithCss)
import Reflex.Localize

import Ergvein.Types.Storage
import Ergvein.Types.Network
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Localization.TestnetDisclaimer
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Page.Restore
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  loadingWidget
  askPasswordModal
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  net <- getNetworkType
  case net of
    Mainnet -> mainnetDispatcher
    Testnet -> testnetDispatcher
    Regtest -> testnetDispatcher

startPage :: MonadFront t m => m ()
startPage = do
  ps <- getPubStorage
  if _pubStorage'restoring ps
    then restorePage
    else balancesPage

testnetDispatcher :: MonadFrontBase t m => m ()
testnetDispatcher = void $ workflow testnetDisclaimer
  where
    testnetDisclaimer = Workflow $ wrapperSimple True $ do
      elClass "h4" "testnet-disclaimer-label" $ dynText =<< localized TestnetDisclaimerLabel
      elClass "p" "testnet-disclaimer-text" $ dynText =<< localized TestnetDisclaimerText
      closeE <- outlineButton TestnetDisclaimerClose
      pure ((), startWallet <$ closeE)
    startWallet = Workflow $ do
      void $ retractStack initialPage `liftAuth` retractStack startPage
      pure ((), never)

mainnetDispatcher :: MonadFrontBase t m => m ()
mainnetDispatcher = void $ retractStack initialPage `liftAuth` retractStack startPage
