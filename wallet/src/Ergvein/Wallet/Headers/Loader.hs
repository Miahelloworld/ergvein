-----------------------------------------------------------------------------
--
-- Module      :  Ergvein.Wallet.Headers.Loader
-- Copyright   :  2019 ATUM SOLUTIONS AG
-- License     :  MIT
--
-- Maintainer  :  Anton Gushcha <ncrashed@protonmail.com>, Vladimir Krutkin <krutkinvs@gmail.com>
-- Stability   :
-- Portability :
--
-- | Contains thread that continuously loads headers from indexers.
--
-----------------------------------------------------------------------------

module Ergvein.Wallet.Headers.Loader (
  headersLoader
) where

import Control.Monad
import Control.Monad.IO.Unlift
import Ergvein.Index.API.Types
import Ergvein.Types.Currency
import Ergvein.Wallet.Client
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Alert

import Haskoin
import Haskoin.Node
import Network.Haskoin.Node.Common
import Network.Socket
import NQE
import Control.Monad.Logger
import           UnliftIO
import           Control.Monad.Trans


testConfig net node_inbox = NodeConfig
                    { nodeConfMaxPeers = 20
                    , nodeConfPeers = []
                    , nodeConfDiscover = True
                    , nodeConfNetAddr = NetworkAddress 0 (SockAddrInet 0 0)
                    , nodeConfNet = net
                    , nodeConfEvents = (`sendSTM` node_inbox)
                    , nodeConfTimeout = 10
                    }

data TestNode = TestNode
    { testMgr    :: Manager
    , testChain  :: Chain
    , nodeEvents :: Inbox NodeEvent
    }

withTestNode ::
       MonadUnliftIO m
    => Network
    -> String
    -> (TestNode -> m a)
    -> m a
withTestNode net str f =
    runNoLoggingT $
    withSystemTempDirectory ("haskoin-node-test-" <> str <> "-") $ \w -> do
        node_inbox <- newInbox
        let cfg =
                NodeConfig
                    { nodeConfMaxPeers = 20
                    , nodeConfPeers = []
                    , nodeConfDiscover = True
                    , nodeConfNetAddr = NetworkAddress 0 (SockAddrInet 0 0)
                    , nodeConfNet = net
                    , nodeConfEvents = (`sendSTM` node_inbox)
                    , nodeConfTimeout = 10
                    }
        withNode cfg $ \(mgr, ch) ->
            lift $
            f TestNode {testMgr = mgr, testChain = ch, nodeEvents = node_inbox}

headersLoader :: (HasHeadersStorage m, MonadFrontBase t m) => m ()
headersLoader = nameSpace "headers loader" $ do
  sequence_ [headersLoaderBtc]

waitForPeer :: MonadIO m => Inbox NodeEvent -> m Peer
waitForPeer inbox =
    receiveMatch inbox $ \case
        PeerEvent (PeerConnected p _) -> Just p
        _ -> Nothing

headersLoaderBtc :: (HasHeadersStorage m, MonadFrontBase t m) => m ()
headersLoaderBtc = nameSpace "btc" $ do
  buildE <- getPostBuild
  he <- handleDangerMsg =<< getHeight (HeightRequest BTC <$ buildE)
  
  let net = btcTest
  x <- liftIO $ withTestNode net "connect-one-peer" $ \TestNode {..} -> do
                p <- waitForPeer nodeEvents
                Just OnlinePeer {onlinePeerVersion = Just Version {version = ver}} <-
                    managerGetPeer p testMgr
                pure ()


  performEvent_ $ ffor he $ \h -> liftIO $ print h
  pure ()
