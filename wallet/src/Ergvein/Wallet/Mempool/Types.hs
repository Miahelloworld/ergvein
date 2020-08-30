module Ergvein.Wallet.Mempool.Types(
    initBtcDbs
  , cleanBtcDbs
  , getBtcFiltersDb
  , getBtcHeightsDb
  , getBtcTotalDb
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


mempoolCacheDbName :: String
mempoolCacheDbName = "mempoolcache"

-- | Force creation of datab
initBtcDbs :: Transaction ReadWrite ()
initBtcDbs = do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  tdb `seq` hdb `seq` fdb `seq` pure ()
  mtotal <- get tdb ()
  let h = filterStartingHeight BTC
  case mtotal of
    Nothing -> put tdb () $ Just h
    Just v -> if h > v then put tdb () $ Just h else pure ()

cleanBtcDbs :: Transaction ReadWrite ()
cleanBtcDbs = do
  clear =<< getMempoolCacheDb

getMempoolCacheDb :: Mode mode => Transaction mode (Database TxHash ByteString)
getMempoolCacheDb = getDatabase $ Just mempoolCacheDbName
