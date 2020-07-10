{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Ergvein.Filters.Btc
import Ergvein.Text
import GHC.Generics
import Network.Haskoin.Address
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Transaction
import System.Random
import Text.Read

import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Ergvein.Filters.Btc.Mutable as M

-- Our benchmark harness.
main = defaultMain [
    -- env makeBlockEnv $ \ ~(txs, block) -> let
    --   bhash = headerHash . blockHeader $ block
    --   n = V.length txs
    --   ptxs = V.drop (n `div` 2) txs -- txs that are in filter
    --   ntxs = V.take (n `div` 2) txs -- txs that not in filter
    --   txsf = makeBtcFilter btcTest (V.toList ptxs) block
    --   in bgroup "scan immutable" [
    --       bench "scan positive address" $ nfIO $ do
    --         addr <- getTxsAddress ptxs
    --         pure $ applyBtcFilter btcTest bhash txsf addr
    --     , bench "scan negative address" $ nfIO $ do
    --         addr <- getTxsAddress ntxs
    --         pure $ applyBtcFilter btcTest bhash txsf addr
    --     , bench "scan all address" $ nfIO $ do
    --         addr <- getTxsAddress txs
    --         pure $ applyBtcFilter btcTest bhash txsf addr
    --     ]
    -- env makeMutBlockEnv $ \ ~(txs, block, txsf) -> let
    --   bhash = headerHash . blockHeader $ block
    --   n = V.length txs
    --   ptxs = V.drop (n `div` 2) txs -- txs that are in filter
    --   ntxs = V.take (n `div` 2) txs -- txs that not in filter
    --   in bgroup "scan mutable" [
    --       bench "scan positive address" $ nfIO $ do
    --         addr <- getTxsAddress ptxs
    --         M.applyBtcFilter btcTest bhash txsf addr
    --     , bench "scan negative address" $ nfIO $ do
    --         addr <- getTxsAddress ntxs
    --         M.applyBtcFilter btcTest bhash txsf addr
    --     , bench "scan all address" $ nfIO $ do
    --         addr <- getTxsAddress txs
    --         M.applyBtcFilter btcTest bhash txsf addr
    --     ]
    env loadTestnetFilters $ \fs -> let
      addr = loadAddress "tb1qjx8u3dz6dnxcnwmpwdcd2c8hzugzt8jap9enpu"
      in bgroup "scan mutable" [
            bench "testnet scan" $ nfIO $ do
              traverse_ (\FiltersRow{..} -> M.applyBtcFilter btcTest rowHash rowFilter addr) fs
          ]
  ]

loadAddress :: Text -> SegWitAddress
loadAddress t =
  fromMaybe (error "Failed to parse address")
    $   guardSegWit
    =<< stringToAddr btcTest t

makeBlockEnv :: IO (Vector Tx, Block)
makeBlockEnv = (,) <$> loadTransactions <*> loadBlock

makeMutBlockEnv :: IO (Vector Tx, Block, M.BtcAddrFilter)
makeMutBlockEnv = do
  txs <- loadTransactions
  block <- loadBlock
  let n = V.length txs
      ptxs = V.drop (n `div` 2) txs
  txsf <- M.makeBtcFilter btcTest (V.toList ptxs) block
  pure (txs, block, txsf)

loadTransactions :: IO (Vector Tx)
loadTransactions = do
  cnt <- T.readFile "./test-transactions"
  pure $ V.fromList $ fmap loadTx $ T.lines cnt
  where
    loadTx = either error id . S.decode @Tx . hex2bs

loadBlock :: IO Block
loadBlock = do
  cnt <- T.readFile "./test-block"
  pure $ either error id . S.decode @Block . hex2bs . T.filter (/= '\n') $ cnt

getTxsAddress :: Vector Tx -> IO SegWitAddress
getTxsAddress txs = do
  addrs <- fmap (V.mapMaybe id) $ traverse getTxAddress txs
  if null addrs then fail "No segwit addresses!" else do
    i <- randomRIO (0, V.length addrs-1)
    pure $ addrs V.! i

getTxAddress :: Tx -> IO (Maybe SegWitAddress)
getTxAddress tx = do
  let addrs = catMaybes $ fmap getSegWitAddr $ txOut tx
  if null addrs then pure Nothing else do
    i <- randomRIO (0, length addrs-1)
    pure $ Just $ addrs !! i

data FiltersRow = FiltersRow {
  rowHeight :: !Int
, rowHash   :: !BlockHash
, rowFilter :: !M.BtcAddrFilter
} deriving (Generic)

instance NFData FiltersRow

parseFiltersRow :: Text -> IO (Maybe FiltersRow)
parseFiltersRow t = case T.splitOn "," t of
  [it, bht, fstr] -> do
    mfilter <- M.decodeBtcAddrFilter (hex2bs fstr)
    pure $ case mfilter of
      Left _ -> Nothing
      Right ft -> do
        i <- readMaybe $ unpack it
        bh <- hexToBlockHash bht
        pure $ FiltersRow i bh ft
  _ -> fail "Wrong count of commas in input file!"

loadTestnetFilters :: IO (Vector FiltersRow)
loadTestnetFilters = do
  cnt <- T.readFile "./testnet-filters"
  rows <- traverse parseFiltersRow $ take 100000 $ T.lines cnt
  let res = V.fromList . catMaybes $ rows
  putStrLn $ "Loaded " <> show (V.length res) <> " filters!"
  pure res
