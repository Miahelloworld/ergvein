{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Filters.Btc.Types(
    initBtcDbs
  , getBtcFiltersDb
  , getBtcHeightsDb
  , getBtcTotalDb
  , getBtcScannedDb
  ) where

import Data.ByteString
import Database.LMDB.Simple
import Network.Haskoin.Block
import Network.Haskoin.Crypto

import Ergvein.Filters.Btc.Mutable
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Wallet.Codec()
import Ergvein.Wallet.Platform

filtersDbName :: String
filtersDbName = "btcfilters"

heightsDbName :: String
heightsDbName = "btcheights"

totalDbName :: String
totalDbName = "btctotal"

scannedDbName :: String
scannedDbName = "btcscanned"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  sdb <- getBtcScannedDb
  tdb `seq` hdb `seq` fdb `seq` sdb `seq` pure ()
  initTotal tdb
  initScaned sdb
  where
    initTotal tdb = do
      mtotal <- get tdb ()
      let h = filterStartingHeight BTC
      case mtotal of
        Nothing -> put tdb () $ Just h
        Just v -> if h > v then put tdb () $ Just h else pure ()
    initScaned sdb = do
      scanned <- get sdb ()
      let h = filterStartingHeight BTC
      case scanned of
        Nothing -> put sdb () $ Just h
        Just v -> if h > v then put sdb () $ Just h else pure ()

getBtcFiltersDb :: Mode mode => Transaction mode (Database BlockHash ByteString)
getBtcFiltersDb = getDatabase $ Just filtersDbName

getBtcHeightsDb :: Mode mode => Transaction mode (Database BlockHeight BlockHash)
getBtcHeightsDb = getDatabase $ Just heightsDbName

getBtcTotalDb :: Mode mode => Transaction mode (Database () BlockHeight)
getBtcTotalDb = getDatabase $ Just totalDbName

getBtcScannedDb :: Mode mode => Transaction mode (Database () BlockHeight)
getBtcScannedDb = getDatabase $ Just scannedDbName
