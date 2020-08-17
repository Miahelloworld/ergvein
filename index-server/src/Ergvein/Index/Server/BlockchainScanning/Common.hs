module Ergvein.Index.Server.BlockchainScanning.Common where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Immortal
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Conversion
import Data.Foldable
import Data.Maybe
import System.DiskSpace
import System.Exit

import Ergvein.Index.Protocol.Types (Message(..), FilterEvent(..))
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Index.Server.Utils
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo    as ERGOScanning
import qualified Network.Bitcoin.Api.Client                      as BitcoinApi

scanningInfo :: ServerM [ScanProgressInfo]
scanningInfo = catMaybes <$> mapM nfo allCurrencies
  where
    nfo :: Currency -> ServerM (Maybe ScanProgressInfo)
    nfo currency = do
      maybeScanned <- getScannedHeight currency
      maybeActual <- (Just <$> actualHeight currency) `catch` (\(SomeException _) -> pure Nothing)
      pure $ ScanProgressInfo currency <$> maybeScanned <*> maybeActual

actualHeight :: Currency -> ServerM BlockHeight
actualHeight currency = case currency of
  BTC  -> BTCScanning.actualHeight
  ERGO -> ERGOScanning.actualHeight

scannerThread :: Currency -> (BlockHeight -> ServerM BlockInfo) -> ServerM Thread
scannerThread currency scanInfo = create $ logOnException . scanIteration
  where
    blockIteration :: BlockHeight -> BlockHeight -> ServerM BlockInfo
    blockIteration totalh blockHeight = do
      let percent = fromIntegral blockHeight / fromIntegral totalh :: Double
      -- logInfoN $ "Scanning height for " <> showt currency <> " " <> showt blockHeight <> " (" <> showf 2 (100*percent) <> "%)"
      scanInfo blockHeight

    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      actual  <- actualHeight currency
      scanned <- getScannedHeight currency
      let toScanFrom = maybe (currencyHeightStart currency) succ scanned
      go toScanFrom actual
      shutdownFlag <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlag $ cfgBlockchainScanDelay cfg
      stopThreadIfShutdown thread
      where
        go current to = do
          shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
          when (not shutdownFlag && current <= to) $ do
            tryBlockInfo <- (Right <$> blockIteration to current)  `catch` (\(SomeException ex) -> pure $ Left $ show ex)
            enoughSpace <- isEnoughSpace
            case tryBlockInfo of
              Right blockInfo | enoughSpace -> do
                maybeLastScannedBlock <- getLastScannedBlock currency
                if flip all maybeLastScannedBlock (== (blockMetaPreviousHeaderBlockHashHexView $ blockInfoMeta blockInfo)) then do --fork detection
                  addBlockInfo blockInfo
                  let BlockMetaInfo{..} = blockInfoMeta blockInfo
                  when (current == to) $ broadcastSocketMessage $ MFiltersEvent $ FilterEvent {
                      filterEventCurrency     = convert blockMetaCurrency
                    , filterEventHeight       = blockMetaBlockHeight
                    , filterEventBlockId      = hex2bs blockMetaHeaderHashHexView
                    , filterEventBlockFilter  = hex2bs blockMetaAddressFilterHexView
                    }
                  go (succ current) to
                else do
                  revertedBlocksCount <- fromIntegral <$> revertContentHistory currency
                  logInfoN $ "Fork detected at " 
                          <> showt current <> " " <> showt currency
                          <> ", performing rollback of " <> showt revertedBlocksCount <> " previous blocks"
                  let restart = (current - revertedBlocksCount)
                  setScannedHeight currency restart
                  go restart to

              _ | not enoughSpace -> 
                logInfoN $ "Not enough available disc space to store block scan result"

              Left errMsg -> do
                 revertedBlocksCount <- fromIntegral <$> revertContentHistory currency
                 logInfoN $ "Error scanning " <> showt current <> " " <> showt currency
                 logInfoN $ showt errMsg
                 logInfoN $ "Performing rollback of " <> showt revertedBlocksCount <> " previous blocks"
                 let restart = (current - revertedBlocksCount)
                 setScannedHeight currency restart
                 go (current - revertedBlocksCount) to

blockchainScanning :: ServerM [Thread]
blockchainScanning = sequenceA
  [ scannerThread BTC  BTCScanning.blockInfo
  ]

feesThread :: ServerM () -> ServerM Thread
feesThread feescan = create $ logOnException . \thread -> do
  feescan
  stopThreadIfShutdown thread

isEnoughSpace :: ServerM Bool
isEnoughSpace = do
  path <- cfgDBPath <$> serverConfig
  availSpace <- liftIO $ getAvailSpace path
  pure $ requiredAvailSpace <= availSpace 
 where
  requiredAvailSpace = 2^30 -- 1Gb

feesScanning :: ServerM [Thread]
feesScanning = sequenceA
  [ feesThread BTCScanning.feeScaner
  ]