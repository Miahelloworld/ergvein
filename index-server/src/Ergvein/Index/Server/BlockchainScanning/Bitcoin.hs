module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Concurrent.Async.Lifted
import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Parallel.Strategies
import           Data.Either
import           Data.Fixed
import           Data.Maybe
import           Data.Serialize
import           Data.Text(Text)
import           Data.Time
import           Data.Word
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Misc
import           Control.Concurrent.STM.TVar

import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.DB.Monad
import           Ergvein.Index.Server.DB.Schema.Filters
import           Ergvein.Index.Server.DB.Serialize
import           Ergvein.Index.Server.DB.Utils
import           Ergvein.Index.Server.Dependencies
import           Ergvein.Index.Server.Monad
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Fees
import           Ergvein.Types.Transaction
import qualified Data.ByteString                    as BS
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Ergvein.Index.Protocol.Types       as IPT
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK

blockTxInfos :: (BitcoinApiMonad m, MonadBaseControl IO m, HasFiltersDB m, MonadLogger m, MonadBaseControl IO m) => BlockHeight -> HK.Block -> m BlockInfo
blockTxInfos txBlockHeight block = do
  let (txInfos , spentTxsIds) = fmap (uniqueWithCount . mconcat) $ unzip $ txInfoFromBlockTx <$> HK.blockTxns block
  -- timeLog $ "spentTxsIds: " <> showt (length spentTxsIds)
  uniqueSpentTxInfos <- fmap mconcat $ mapConcurrently (mapM spentTxInfo) $ mkChunks 100 spentTxsIds
  let uniqueSpentTxs = txStoredTx <$> uniqueSpentTxInfos
  blockAddressFilter <- encodeBtcAddrFilter =<<
    withInputTxs uniqueSpentTxs (makeBtcFilter isErgveinIndexable block)
  let blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.prevBlock $ HK.blockHeader block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHash prevBlockHeaderHash blockAddressFilter
  let spentTxsIdsMap = Map.mapKeys hkTxHashToEgv $ Map.fromList spentTxsIds
  pure $ BlockInfo blockMeta [] txInfos
  where
    blockTxMap = mapBy (HK.txHash) $ HK.blockTxns block
    spentTxInfo :: (MonadBaseControl IO m, BitcoinApiMonad m, HasFiltersDB m, MonadLogger m) => (HK.TxHash, Word32) -> m TxInfoStored
    spentTxInfo (txInId, spent) = case Map.lookup txInId blockTxMap of
      Just    sourceTx -> pure $ txInfoFromMapTx txInId sourceTx spent
      Nothing          -> do
        etx <- getTxInfoFromCache txInId spent
        case etx of
          Left _ -> do
            logWarnN $ "[blockTxInfos]: Failed to get a Tx from DB. Trying the node. " <> showt txInId
            getTxInfoFromNode txInId
          Right tx -> pure tx

    txInfoFromBlockTx :: HK.Tx -> (TxInfo, [HK.TxHash])
    txInfoFromBlockTx tx = let
      info = TxInfo { txInfoHash    = hkTxHashToEgv $ HK.txHash tx
                    , txInfoBytes   = egvSerialize BTC tx
                    , txInfoUnspent = calcUnspentOuts tx
                    }
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint)
      spentTxInfo = HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in (info, spentTxInfo)

    txInfoFromMapTx :: HK.TxHash -> HK.Tx -> Word32 -> TxInfoStored
    txInfoFromMapTx thash tx spent =
      let unsp = calcUnspentOuts tx - spent
      in TxInfoStored {
          txStoredHash    = hkTxHashToEgv thash
        , txStoredTx      = tx
        , txStoredHeight  = fromIntegral txBlockHeight
        , txStoredMeta    = if unsp <= 0 then Nothing else Just $ TxRecMeta unsp $ egvSerialize BTC tx
        }

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

getTxInfoFromCache :: (HasFiltersDB m, MonadLogger m)
  => HK.TxHash -> Word32 -> m (Either String TxInfoStored)
getTxInfoFromCache thash spent = do
  db <- getFiltersDb
  let egvTHash = hkTxHashToEgv thash
  TxRec{..} <- getParsedExact BTC "getTxInfoFromCache" db $ txRecKey egvTHash
  pure $ case txRecMeta of
    Nothing -> Left "No record with given TxHash. Fallback"
    Just TxRecMeta{..} -> case egvDeserialize BTC txMetaBytes of
      Left err -> Left err
      Right (tx :: HK.Tx) -> let unsp = txMetaUnspent - spent in
        Right $ TxInfoStored egvTHash tx txRecHeight $
          if unsp <= 0 then Nothing else Just $ TxRecMeta unsp txMetaBytes

getTxInfoFromNode :: (BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, HasFiltersDB m)
  => HK.TxHash -> m TxInfoStored
getTxInfoFromNode thash = do
  let egvTHash = hkTxHashToEgv thash
  db <- getFiltersDb
  txrec <- getParsedExact BTC "getTxInfoFromNode" db $ txRecKey egvTHash
  blk <- getBtcBlock $ fromIntegral $ txRecHeight txrec
  let txChunks = mkChunks 100 $ HK.blockTxns blk
  txs <- fmap mconcat $ mapConcurrently (pure . catMaybes . parMap rpar comparator) txChunks
  case txs of
    [] -> txGettingError $ txRecHeight txrec
    tx:_ -> pure $ TxInfoStored egvTHash tx (txRecHeight txrec) Nothing
  where
    comparator tx = if thash == HK.txHash tx then Just tx else Nothing
    txGettingError h = error $ "Failed to get tx from block #" ++ show h ++ " TxHash: " ++ show thash

getBtcBlock :: (BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, MonadIO m)
  => BlockHeight -> m HK.Block
getBtcBlock blockHeightReq = do
  blockHash <- nodeRpcCall $ (`getBlockHash` fromIntegral blockHeightReq)
  conScheme <- getBtcConnectionScheme
  case conScheme of
    BtcConTCP -> requestBlock $ fromRight hashParsingError $ decode $ BS.reverse $ HS.toBytes blockHash
    BtcConRPC -> do
      maybeRawBlock <- nodeRpcCall $ (`getBlockRaw` blockHash)
      let rawBlock = fromMaybe blockParsingError maybeRawBlock
      pure $ fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  where
    hashParsingError = error $ "Error parsing BTC BlockHash at height " ++ show blockHeightReq
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightReq
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightReq

blockInfo :: (BitcoinApiMonad m, HasFiltersDB m, MonadLogger m, MonadBaseControl IO m)
  => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan = blockTxInfos blockHeightToScan =<< getBtcBlock blockHeightToScan

feeScaner :: ServerM ()
feeScaner = feeScaner' 0
  where
    feeScaner' :: BlockHeight -> ServerM ()
    feeScaner' h = do
      cfg <- serverConfig
      h'  <- actualHeight
      h'' <- if h' == h
        then pure h'
        else do
          res <- fmap catMaybes $ flip traverse [FeeFast, FeeModerate, FeeCheap] $ \lvl -> do
            let req mode c = estimateSmartFee c (fromIntegral $ feeTargetBlocks BTC lvl) mode
            mco <- nodeRpcCall $ req Conservative
            mec <- nodeRpcCall $ req Economical
            case (estimateResFee mco, estimateResFee mec) of
              (Just (MkFixed co), Just (MkFixed ec)) -> pure $ Just (lvl, (fromIntegral co `div` 1000 , fromIntegral ec `div` 1000))
              _ -> pure Nothing
          setFees IPT.BTC $ mkFeeBundle res
          logInfoN $ "[BTC]: " <> showt res
          pure $ case res of
            [] -> h
            _  -> h'
      shutdownFlagVar <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlagVar $ cfgFeeEstimateDelay cfg
      shutdownFlag <- liftIO $ readTVarIO shutdownFlagVar
      unless shutdownFlag $ feeScaner' h''

timeLog :: (MonadLogger m, MonadIO m) => Text -> m ()
timeLog t = do
  now <- liftIO $ getCurrentTime
  logInfoN $ "["<> showt now <> "] " <> t

calcUnspentOuts :: HK.Tx -> Word32
calcUnspentOuts = fromIntegral . length . filter withoutDataCarrier . HK.txOut
{-# INLINE calcUnspentOuts #-}

withoutDataCarrier :: HK.TxOut -> Bool
withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
{-# INLINE withoutDataCarrier #-}
