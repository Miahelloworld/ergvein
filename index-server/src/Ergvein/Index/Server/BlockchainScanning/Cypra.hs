-- |
module Ergvein.Index.Server.BlockchainScanning.Cypra where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Immortal

import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.BlockchainScanning.Types

import HSChain.Control.Channels (awaitIO)
import qualified HSChain.Network.TCP        as PoW
import qualified HSChain.Store.Query        as PoW
import qualified HSChain.Logger             as PoW
import qualified HSChain.PoW.P2P            as PoW
import qualified HSChain.PoW.P2P.Types      as PoW
import qualified HSChain.PoW.Types          as PoW
import qualified HSChain.PoW.Consensus      as PoW
import qualified Hschain.Utxo.Pow.App       as Cypra
import qualified Hschain.Utxo.Pow.App.Types as Cypra

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


scanBlock :: PoW.Block (Cypra.UTXOBlock t) -> BlockInfo
scanBlock = undefined
