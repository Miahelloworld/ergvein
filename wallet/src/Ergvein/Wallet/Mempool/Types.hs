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
import Network.Haskoin.Transaction

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
  mdb <- getMempoolCacheDb
  put mdb ()

cleanBtcDbs :: Transaction ReadWrite ()
cleanBtcDbs = do
  clear =<< getMempoolCacheDb

getMempoolCacheDb :: Mode mode => Transaction mode (Database TxHash ByteString)
getMempoolCacheDb = getDatabase $ Just mempoolCacheDbName
