module Ergvein.Index.Server.BlockchainScanning.Ergo where

import  Control.Monad.Reader
import  Data.Maybe

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Interfaces.Ergo.Api
import Ergvein.Interfaces.Ergo.It.Api.NodeApi
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import           Network.Ergo.Api.Blocks
import           Network.Ergo.Api.Client
import           Network.Ergo.Api.Info
import qualified Network.Ergo.Api.Utxo    as UtxoApi
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M


txInfo :: ApiMonad m => ErgoTransaction -> m ([TxInfo], [TxHash])
txInfo tx = do
  pure ([], []) -- TODO: fix this when adding ERGO
  -- let info = TxInfo { txInfoHash =  ErgTxHash . ErgTxId $ BSS.toShort $ unTransactionId $ transactionId (tx :: ErgoTransaction)
  --                   , txInfoBytes = mempty
  --                   , txInfoUnspent = fromIntegral $ length $ dataInputs tx
  --                   }
  -- txIns <- forM (dataInputs tx) txInInfo
  -- let spentTxIds = ErgTxHash . ErgTxId . BSS.toShort . unTransactionId . fromJust . (transactionId :: ErgoTransactionOutput -> Maybe TransactionId) <$> txIns
  -- pure $ ([info], spentTxIds)
  -- where
  --   txInInfo txIn = UtxoApi.getById $ boxId (txIn :: ErgoTransactionDataInput)

blockTxInfos :: ApiMonad m => FullBlock -> BlockHeight -> m BlockInfo
blockTxInfos block txBlockHeight = do
  (txInfos , spentTxsIds) <- mconcat <$> mapM txInfo (transactions $ blockTransactions block)
  let blockHeaderHash = mempty --TODO
      prevBlockHeaderHash = mempty --TODO
      blockAddressFilter = mempty --TODO
      spentMap = M.fromList $ (,0) <$> spentTxsIds --TODO
      blockMeta = BlockMetaInfo ERGO (fromIntegral txBlockHeight) blockHeaderHash prevBlockHeaderHash blockAddressFilter
  pure $ BlockInfo blockMeta [] txInfos

actualHeight :: ApiMonad m => m BlockHeight
actualHeight = do
    info <- getInfo
    pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight info

blockInfo :: ApiMonad m  => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan = do
  headersAtHeight <- getHeaderIdsAtHeight
      $ Height
      $ fromIntegral blockHeightToScan

  let mainChainId = head headersAtHeight

  block <- getById mainChainId
  blockTxInfos block blockHeightToScan
