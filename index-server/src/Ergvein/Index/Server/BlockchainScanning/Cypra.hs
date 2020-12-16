-- |
module Ergvein.Index.Server.BlockchainScanning.Cypra where

import Codec.Serialise (serialise)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Immortal
import Data.Proxy
-- import Data.Maybe
import Data.Foldable
import qualified Data.HashSet          as HS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Map.Strict       as Map
import qualified Haskoin   as HK
import Unsafe.Coerce

import HSChain.Control.Channels (awaitIO)
import qualified HSChain.Crypto             as PoW
import qualified HSChain.Network.TCP        as PoW
import qualified HSChain.Store.Query        as PoW
import qualified HSChain.Logger             as PoW
import qualified HSChain.PoW.P2P            as PoW
import qualified HSChain.PoW.P2P.Types      as PoW
import qualified HSChain.PoW.Types          as PoW
import qualified HSChain.PoW.Consensus      as PoW
import qualified Hschain.Utxo.Pow.App       as Cypra
import qualified Hschain.Utxo.Pow.App.Types as Cypra
import qualified Hschain.Utxo.Lang.Types    as Cypra

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Utils
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Filters.Btc       (BtcAddrFilter(..),encodeBtcAddrFilter)
import Ergvein.Filters.Btc.Index
import Ergvein.Filters.GCS

scanThread :: ServerM Thread
scanThread = create $ \_ -> do
  PoW.withConnection dbPath $ \conn ->
    PoW.withLogEnv "" "" (PoW.makeScribe <$> loggers) $ \logEnv ->
      Cypra.runUTXOT logEnv conn $ evalContT $ do
        (db, bIdx, _sView) <- lift $ Cypra.utxoStateView Cypra.genesisMock
        let c0 = PoW.createLightConsensus Cypra.genesisMock bIdx
        pow <- PoW.lightNode netcfg net db c0
        -- Report to stdout
        void $ liftIO $ forkIO $ do
          ch <- atomically $ PoW.bestHeadUpdates pow
          forever $ do bh <- fst . PoW._bestLightHead <$> awaitIO ch
                       print (PoW.bhHeight bh, PoW.bhBID bh)
                       print $ PoW.retarget bh
        --
        liftIO $ forever $ threadDelay maxBound
        return ()
  where
    dbPath  = ""
    loggers = [ PoW.ScribeSpec PoW.ScribeJSON Nothing PoW.DebugS PoW.V2
              ]
    net     = PoW.newNetworkTcp 10101
    netcfg  = PoW.NodeCfg { PoW.nKnownPeers     = 1
                          , PoW.nConnectedPeers = 1
                          , PoW.initialPeers    = [ read "192.168.1.4:40000" ]
                          }


scanBlock
  :: forall t m. (Cypra.UtxoPOWConfig t, HasTxIndex m)
  => PoW.Block (Cypra.UTXOBlock t) -> m BlockInfo
scanBlock b = do
  flt <- encodeBtcAddrFilter <$> makeCypFilter b
  pure $ BlockInfo
    { blockInfoMeta       = BlockMetaInfo
      { blockMetaCurrency                = CYPRA
      , blockMetaBlockHeight             = let PoW.Height h = PoW.blockHeight b
                                           in fromIntegral h
      , blockMetaHeaderHash              = encodeShort $ PoW.blockID b
      , blockMetaPreviousHeaderBlockHash = encodeShort $ case PoW.prevBlock b of
          Nothing  -> error "scanBlock: genesis block"
          Just bid -> bid
      , blockMetaAddressFilter           = flt
      }
    , spentTxOutputs      = Map.fromList spentTxsIds
    , blockContentTxInfos = txInfos
    }
  where
    txInfos     = fmap (txInfo (Proxy @t)) $ toList $ Cypra.ubData $ PoW.blockData b
    spentTxsIds = uniqueWithCount
                $ spentInputs (Proxy @t) =<< toList (Cypra.ubData $ PoW.blockData b)

txInfo :: forall t. Cypra.UtxoPOWConfig t => Proxy t -> PoW.Tx (Cypra.UTXOBlock t) -> TxInfo
-- FIXME: Tx as type family breaks type inference. Badly!
txInfo _ tx = TxInfo
  { txHash         = CypTxHash $ CypTxId $ encodeShort $ PoW.txID @(Cypra.UTXOBlock t) tx
  , txBytes        = BL.toStrict $ serialise tx
  , txOutputsCount = fromIntegral $ length $ Cypra.tx'outputs tx
  }

spentInputs :: forall t. Cypra.UtxoPOWConfig t => Proxy t -> PoW.Tx (Cypra.UTXOBlock t) -> [TxHash]
spentInputs _
  -- FIXME: We need to number outputs explicitly
  = undefined
  -- = map (unsafeCoerce . BSS.toShort . PoW.encodeToBS . undefined)
  -- . toList . Cypra.tx'inputs


encodeShort :: PoW.ByteRepr a => a -> BSS.ShortByteString
encodeShort = BSS.toShort . PoW.encodeToBS


----------------------------------------------------------------
-- Making of filters
--
-- Implemented here for ease of development. To be moved into erg

makeCypFilter
  :: (HasTxIndex m, Cypra.UtxoPOWConfig t)
  => PoW.Block (Cypra.UTXOBlock t) -> m BtcAddrFilter
makeCypFilter b = do
  -- Fetch inputs for each transacion
  inputSet <- collectInputs b
  let totalSet = HS.fromList $ outputSet <> inputSet
  pure BtcAddrFilter
      { btcAddrFilterN   = undefined
      , btcAddrFilterGcs = constructGcs btcDefP sipkey btcDefM totalSet
      }
  where
    -- Set of all outputs of transactions in block
    outputSet = concatMap (fmap encodeOut . toList . Cypra.tx'outputs)
              $ toList
              $ Cypra.ubData $ PoW.blockData b
    encodeOut Cypra.Box{..} = BL.toStrict $ serialise (box'script, box'args)
    -- FIXME: Sadly there's no way to construct Hash256. So we have to
    --        resort to unsafeCoerce in order to convert hashes.
    sipkey    = blockSipHash $ HK.BlockHash $ unsafeCoerce $ BSS.toShort $ PoW.encodeToBS $ PoW.blockID b


collectInputs
  :: (HasTxIndex m, Cypra.UtxoPOWConfig t)
  => PoW.Block (Cypra.UTXOBlock t) -> m [BS.ByteString]
collectInputs _ = pure []
-- FIXME: We should use same schema as bitcoin
{-
  = fmap catMaybes
  . mapM queryOutPoint
  . fmap undefined
  . concatMap (toList . Cypra.tx'inputs)
  . drop 1 . toList . Cypra.ubData . PoW.blockData
-}
