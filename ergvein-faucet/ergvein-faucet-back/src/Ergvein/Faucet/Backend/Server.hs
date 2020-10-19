module Ergvein.Faucet.Backend.Server(
    ergveinFaucetServer
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Ergvein.Faucet.API
import Ergvein.Faucet.Backend.Btc.Client
import Ergvein.Faucet.Backend.Monad

import qualified Data.Base58String.Bitcoin as B58
import qualified Data.HexString as HX
import qualified Data.Text.Encoding as TE
import qualified Network.Bitcoin.Api.Blockchain as C
import qualified Network.Bitcoin.Api.Types.TxInfo as BI
import qualified Network.Bitcoin.Api.Wallet as C

ergveinFaucetServer :: XenoFaucetAPI AsServerM
ergveinFaucetServer = XenoFaucetAPI {
    getBalanceEndpoint            = getBalanceEndpointImpl
  , mineBlocksEndpoint            = mineBlocksEndpointImpl
  , getHeightEndpoint             = getHeightEndpointImpl
  , sendToAddressEndpoint         = sendToAddressEndpointImpl
  , setBlockIntervalEndpoint      = setBlockIntervalEndpointImpl
  , getBlockIntervalEndpoint      = getBlockIntervalEndpointImpl
  , getTxConfirmationsEndpoint    = getTxConfirmationsEndpointImpl
  }

getBalanceEndpointImpl :: Currency -> ServerM Double
getBalanceEndpointImpl BTC = fmap realToFrac $ runBtcRpc "balance" $ \c -> getBalance c
getBalanceEndpointImpl cur = fail $ "getBalance unimplemented for " <> show cur

mineBlocksEndpointImpl :: Currency -> Word -> ServerM ()
mineBlocksEndpointImpl BTC n = void $ runBtcRpc "generate" $ \c -> do
  addr <- newAddress c
  generateToAddress c (fromIntegral n) addr
mineBlocksEndpointImpl cur _ = fail $ "mineBlocks unimplemented for " <> show cur

getHeightEndpointImpl :: Currency -> ServerM Word
getHeightEndpointImpl BTC = fmap fromIntegral $ runBtcRpc "height" $ \c -> getBlockCount c
getHeightEndpointImpl cur = fail $ "getHeight unimplemented for " <> show cur

sendToAddressEndpointImpl :: Currency -> SendToAddress -> ServerM Text
sendToAddressEndpointImpl BTC SendToAddress{..} = runBtcRpc "send address" $ \c -> do
  tid <- C.sendToAddress c sendToAddressAddress (realToFrac sendToAddressAmount)
  pure $ HX.toText tid
sendToAddressEndpointImpl cur _ = fail $ "mineBlocks unimplemented for " <> show cur

setBlockIntervalEndpointImpl :: Currency -> Maybe NominalDiffTime -> ServerM ()
setBlockIntervalEndpointImpl BTC dt = do
  ch <- asks envBlockIntervalChan
  var <- asks envBlockIntervalVar
  liftIO $ do
    writeChan ch dt
    writeIORef var dt
setBlockIntervalEndpointImpl cur _ = fail $ "setBlockInterval unimplemented for " <> show cur

getBlockIntervalEndpointImpl :: Currency -> ServerM (Maybe NominalDiffTime)
getBlockIntervalEndpointImpl BTC = do
  var <- asks envBlockIntervalVar
  liftIO $ readIORef var
getBlockIntervalEndpointImpl cur = fail $ "getBlockInterval unimplemented for " <> show cur

getTxConfirmationsEndpointImpl :: Currency -> Maybe Text -> ServerM (Maybe Int)
getTxConfirmationsEndpointImpl _ Nothing = pure Nothing
getTxConfirmationsEndpointImpl BTC (Just tid) = runBtcRpc "tx confirmations" $ \c -> do
  let thash = HX.hexString . TE.encodeUtf8 $ tid
  minfo <- C.getTransaction c thash
  case minfo of
    Nothing -> pure Nothing
    Just BI.RawTxInfo{..} -> pure $ Just $ fromIntegral rawTxInfoConfirmations
getTxConfirmationsEndpointImpl cur _ = fail $ "getTxConfirmations unimplemented for " <> show cur
