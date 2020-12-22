-- |
module Ergvein.Index.Server.BlockchainScanning.Cypra where

import Codec.Serialise (serialise,deserialiseOrFail)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Immortal
import Control.Lens
import Data.Proxy
import Data.Coerce
import Data.Maybe
import Data.Foldable
import qualified Data.HashSet          as HS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Map.Strict       as Map
import qualified Haskoin   as HK
import Unsafe.Coerce

import HSChain.Control.Channels (awaitIO,await)
import HSChain.Control.Util     (atomicallyIO)
import qualified HSChain.Crypto             as PoW
import qualified HSChain.Crypto.SHA         as PoW
import qualified HSChain.Network.TCP        as PoW
import qualified HSChain.Store.Query        as PoW
import qualified HSChain.Logger             as PoW
import qualified HSChain.PoW.P2P            as PoW
import qualified HSChain.PoW.P2P.Types      as PoW
import qualified HSChain.PoW.Types          as PoW
import qualified HSChain.PoW.BlockIndex     as PoW
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
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema.Filters

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
                       print ("HEAD"::String,PoW.bhHeight bh, PoW.bhBID bh)
        -- Main loop
        -- FIXME: Here we _always_ start from genesis
        let Just bh0 = PoW.lookupIdx (PoW.blockID Cypra.genesisMock) bIdx
            st0 = IdxState { currentHead   = bh0
                           , blocksToFetch = []
                           }
        headUpdates <- atomicallyIO $ PoW.bestHeadUpdates pow
        let loop waitBlk st = do
              evt <- atomicallyIO
                   $  view (PoW.bestLightHead . _1 . to NewHead) <$> await headUpdates
                  <|> NewBlock <$> waitBlk
              (st',cmd) <- lift $ transition st evt
              liftIO $ print cmd
              case cmd of
                NoOp           -> do loop waitBlk st'
                FetchBlock bid -> do waitBlk' <- atomicallyIO $ PoW.fetchBlock pow bid
                                     loop waitBlk' st'
        lift $ loop retry st0
  where
    dbPath  = ""
    loggers = [ PoW.ScribeSpec PoW.ScribeJSON (Just "./cypralog") PoW.DebugS PoW.V2
              ]
    net     = PoW.newNetworkTcp 10101
    netcfg  = PoW.NodeCfg { PoW.nKnownPeers     = 1
                          , PoW.nConnectedPeers = 1
                          , PoW.initialPeers    = [ read "192.168.1.4:40000" ]
                          }


type CypBCh = Cypra.UTXOBlock Cypra.MockChain

-- | State of indexer
data IdxState = IdxState
  { currentHead :: PoW.BH CypBCh
    -- ^ Last block we've fetched and processes.
  , blocksToFetch :: [PoW.BH CypBCh]
    -- ^ List of blocks we need to fetch.
  }
  deriving Show

data Event
  = NewHead  (PoW.BH CypBCh)
  -- ^ We've got new head
  | NewBlock (PoW.Block CypBCh)
  -- ^ We've fetched new block
  deriving Show

data Command
  = NoOp
  | FetchBlock (PoW.BlockID CypBCh)
  deriving Show

transition
  :: (HasBtcRollback m, HasFiltersDB m, HasIndexerDB m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
  => IdxState -> Event -> m (IdxState, Command)
-- We've got new head.
transition IdxState{..} (NewHead newHead) = do
  -- FIXME: We don't implement rollback
  let toPath (PoW.RevertBlock _ _ )  = error "Cypra: rollback is not implemented"
      toPath  PoW.NoChange           = []
      toPath (PoW.ApplyBlock bid xs) = bid : toPath xs
  -- liftIO $ print $ PoW.bhHeight <$> path
  let newBlocks = reverse $ toPath path
  pure ( IdxState { blocksToFetch = newBlocks
                  , ..
                  }
       , case (blocksToFetch, newBlocks) of
           (_     , []    )                -> NoOp
           (bOld:_, bNew:_) | bOld == bNew -> NoOp
           (_     , bNew:_)                -> FetchBlock (PoW.bhBID bNew)
       )
  where
    path = PoW.makeBlockIndexPath id currentHead newHead
-- We've got new block
transition st@IdxState{..} (NewBlock blk)
  | bh:rest <- blocksToFetch
  , PoW.bhBID bh == PoW.blockID blk = do
      liftIO $ print ("BLOCK"::String,PoW.bhHeight bh, PoW.bhBID bh)
      addBlockInfo =<< scanBlock blk
      pure ( IdxState { currentHead   = bh
                      , blocksToFetch = rest
                      }
           , case rest of
               []    -> NoOp
               (b:_) -> FetchBlock $ PoW.bhBID b
           )
  | otherwise = pure (st, NoOp)


scanBlock
  :: forall t m. (Cypra.UtxoPOWConfig t, HasFiltersDB m, MonadLogger m)
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

-- Hashes of transaction which created spent output
spentInputs :: forall t. Cypra.UtxoPOWConfig t => Proxy t -> PoW.Tx (Cypra.UTXOBlock t) -> [TxHash]
spentInputs _ tx
  = tx ^.. Cypra.tx'inputsL . each . Cypra.boxInputRef'idL . to toCypTxHash


encodeShort :: PoW.ByteRepr a => a -> BSS.ShortByteString
encodeShort = BSS.toShort . PoW.encodeToBS

toCypTxHash :: Cypra.BoxId -> TxHash
toCypTxHash (Cypra.BoxId tid _) = cypTxHash tid

cypTxHash :: Cypra.TxId -> TxHash
cypTxHash (Cypra.TxId h) = CypTxHash $ CypTxId $ encodeShort h

----------------------------------------------------------------
-- Making of filters
--
-- Implemented here for ease of development. To be moved into erg

makeCypFilter
  :: (HasFiltersDB m, MonadLogger m, Cypra.UtxoPOWConfig t)
  => PoW.Block (Cypra.UTXOBlock t) -> m BtcAddrFilter
makeCypFilter b = do
  -- Fetch inputs for each transacion
  inputSet <- collectInputs b
  let totalSet = HS.fromList $ outputSet <> inputSet
  pure BtcAddrFilter
      { btcAddrFilterN   = fromIntegral $ length totalSet
      , btcAddrFilterGcs = constructGcs btcDefP sipkey btcDefM totalSet
      }
  where
    -- Set of all outputs of transactions in block
    outputSet = concatMap (fmap encodeOut . toList . Cypra.tx'outputs)
              $ toList
              $ Cypra.ubData $ PoW.blockData b
    encodeOut Cypra.Box{..} = BL.toStrict $ serialise (box'script, box'args)
    sipkey    = blockSipHash $ HK.BlockHash $ toHKhash $ coerce $ PoW.blockID b


-- FIXME: Sadly there's no way to construct Hash256. So we have to
--        resort to unsafeCoerce in order to convert hashes.
toHKhash :: PoW.Hash PoW.SHA256 -> HK.Hash256
toHKhash = unsafeCoerce . BSS.toShort . PoW.encodeToBS


collectInputs
  :: forall m t. (HasFiltersDB m, MonadLogger m, Cypra.UtxoPOWConfig t)
  => PoW.Block (Cypra.UTXOBlock t) -> m [BS.ByteString]
collectInputs b = do
  txs <- traverse fetchSource $ spentTIDs
  pure $ BL.toStrict . serialise <$> catMaybes txs
  where
    -- FIXME: Do something about fact that TxId & TxID are nominally different
    blockTxMap = mapBy (cypTxHash . coerce . PoW.txID @(Cypra.UTXOBlock t))
               $ toList $ Cypra.ubData $ PoW.blockData b
    -- Hashes of transaction which had spent outputs
    spentTIDs  = concatMap (toListOf $ Cypra.tx'inputsL . each . Cypra.boxInputRef'idL . to toCypTxHash)
               $ drop 1 $ toList $ Cypra.ubData $ PoW.blockData b
    -- Fetch transaction from database
    fetchSource tid = do
      case tid `Map.lookup` blockTxMap of
        Just _  -> pure Nothing
        Nothing -> Just <$> fromCache tid

fromCache :: (HasFiltersDB m, MonadLogger m) => TxHash -> m (PoW.Tx (Cypra.UTXOBlock t))
fromCache txInId = do
  db  <- getFiltersDb
  src <- getParsedExact CYPRA "blockTxInfos" db $ txRawKey txInId
  case deserialiseOrFail $ BL.fromStrict $ unTxRecBytes src of
    Left err -> error (show err <> " : " <> show src)
    Right tx -> pure tx
