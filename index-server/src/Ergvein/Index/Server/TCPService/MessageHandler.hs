module Ergvein.Index.Server.TCPService.MessageHandler where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion
import Data.Time.Clock.POSIX
import Control.Monad.Random

import Ergvein.Index.API.Types
import Ergvein.Index.Protocol.Types as IPT
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Common
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.PeerDiscovery.Types
import Network.Socket
import Ergvein.Index.Server.DB.Queries

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

getBlockMetaSlice :: Currency -> BlockHeight -> BlockHeight -> ServerM [BlockMetaRec]
getBlockMetaSlice currency startHeight endHeight = do
  db <- getFiltersDb
  let start = metaRecKey (currency, startHeight)
      end   = BlockMetaRecKey currency $ startHeight + pred endHeight
  slice <- safeEntrySlice db start end
  let metaSlice = snd <$> slice
  pure metaSlice

handleMsg :: SockAddr -> Message -> ServerM [Message]
handleMsg address (MPing msg) = pure [MPong msg]

handleMsg address (MPong _) = pure mempty

handleMsg address (MVersionACK _) = pure mempty

handleMsg address (MVersion Version{..}) = do

 -- pMatch <- isProgressMatch
  if protocolVersion == versionVersion then do
    currentTime <- liftIO getCurrentTime
    addPeer1 $ Peer1 address currentTime
    ownVer <- ownVersion
    pure [ MVersionACK $ VersionACK, MVersion ownVer ]
  else
    pure mempty

handleMsg address (MPeerRequest _) = do
  knownPeers <- getKnownPeers
  pure $ pure $ MPeerResponse $ PeerResponse $ V.fromList knownPeers

handleMsg address (MFiltersRequest FilterRequest {..}) = do
  let currency = convert filterRequestMsgCurrency
  slice <- getBlockMetaSlice currency filterRequestMsgStart filterRequestMsgAmount
  let filters = V.fromList $ convert <$> slice

  pure $ pure $ MFiltersResponse $ FilterResponse
    { filterResponseCurrency = filterRequestMsgCurrency
    , filterResponseFilters = filters
    }

handleMsg address (MFeeRequest curs) = do
  fees <- liftIO . readTVarIO =<< asks envFeeEstimates
  let selCurs = M.restrictKeys fees $ S.fromList curs
  let resps =  (`M.mapWithKey` selCurs) $ \cur fb -> case cur of
        IPT.BTC -> FeeRespBTC False fb
        IPT.TBTC -> FeeRespBTC True fb
        _ -> let FeeBundle (_, h) (_, m) (_, l) = fb
          in FeeRespGeneric cur h m l
  pure $ pure $ MFeeResponse $ M.elems resps

ownVersion :: ServerM Version
ownVersion = do
  now   <- liftIO $ round <$> getPOSIXTime
  nonce <- liftIO $ randomIO
  time  <- liftIO $ fromIntegral . floor <$> getPOSIXTime

  scanNfo <- UV.fromList . fmap convert <$> scanningInfo

  pure $ Version {
      versionVersion    = protocolVersion
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = scanNfo
    }
