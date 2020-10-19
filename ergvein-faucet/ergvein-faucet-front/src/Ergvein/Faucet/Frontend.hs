module Ergvein.Faucet.Frontend(
    frontend
  ) where

import Control.Monad
import Control.Monad.Fix
import Reflex.Dom
import Ergvein.Faucet.API hiding (sendToAddress)
import Ergvein.Faucet.Frontend.Client
import Ergvein.Faucet.Frontend.Elements
import Ergvein.Faucet.Frontend.Inputs
import Ergvein.Faucet.Frontend.Monad
import Ergvein.Faucet.Shared

frontend :: MonadWidget t m => m ()
frontend = container $ do
  h3 $ text "Crosschain testnet BTC faucet"
  heightWidget
  balanceWidget
  mineWidget
  sendWidget
  intervalWidget

heightWidget :: MonadWidget t m => m ()
heightWidget = do
  buildE <- getPostBuild
  e <- tickLossyFromPostBuildTime 3
  heightE <- getHeight $ BTC <$ leftmost [void e, buildE]
  heightD <- holdDyn 0 heightE
  h4 $ dynText $ ("Height " <>) . showt <$> heightD

balanceWidget :: MonadWidget t m => m ()
balanceWidget = do
  buildE <- getPostBuild
  e <- tickLossyFromPostBuildTime 3
  balanceE <- getBalance $ BTC <$ leftmost [void e, buildE]
  balanceD <- holdDyn 0 balanceE
  h4 $ dynText $ ("Balance " <>) . showt <$> balanceD

mineWidget :: MonadWidget t m => m ()
mineWidget = do
  h4 $ text "Mine"
  countD <- readField "Blocks" $ pure 1
  mineE <- button "Mine"
  void $ mineBlocks countD (BTC <$ mineE)

sendWidget :: MonadWidget t m => m ()
sendWidget = do
  h4 $ text "Send"
  addressD <- textField "Address" $ pure ""
  amountD <- readField "Amount" $ pure 0
  sendE <- button "Send"
  resE <- sendToAddress (fmap Right $ SendToAddress <$> addressD <*> amountD) (BTC <$ sendE)
  _ <- widgetHold (pure ()) $ ffor resE $ \txid -> h5 $ text txid
  pure ()

intervalWidget :: forall t m . MonadWidget t m => m ()
intervalWidget = do
  h4 $ text "Mining interval"

  buildE <- getPostBuild
  e <- tickLossyFromPostBuildTime 3
  intE <- getBlockInterval $ BTC <$ leftmost [void e, buildE]
  intD <- holdDyn Nothing intE
  h4 $ dynText $ ("Mine interval " <>) . showt <$> intD

  secondsD :: Dynamic t (Maybe Double) <- readMayField "Seconds" $ pure Nothing
  sendE <- button "Send"
  _ <- setBlockInterval (fmap realToFrac <$> secondsD) (BTC <$ sendE)
  pure ()
