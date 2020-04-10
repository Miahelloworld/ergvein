{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Haskoin.NodeSpec
    ( spec
    ) where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Maybe
import           Haskoin
import           Haskoin.Node
import           Network.Socket       (SockAddr (..))
import           NQE
import           Test.Hspec
import           UnliftIO

data TestNode = TestNode
    { testMgr    :: Manager
    , testChain  :: Chain
    , nodeEvents :: Inbox NodeEvent
    }

spec :: Spec
spec = do
    let net = btcTest
    describe "peer manager on test network" $ do
        it "connects to a peer" $
            withTestNode net "connect-one-peer" $ \TestNode {..} -> do
                p <- waitForPeer nodeEvents
                Just OnlinePeer {onlinePeerVersion = Just Version {version = ver}} <-
                    managerGetPeer p testMgr
                ver `shouldSatisfy` (>= 70002)

waitForPeer :: MonadIO m => Inbox NodeEvent -> m Peer
waitForPeer inbox =
    receiveMatch inbox $ \case
        PeerEvent (PeerConnected p _) -> Just p
        _ -> Nothing


withTestNode ::
       MonadUnliftIO m
    => Network
    -> String
    -> (TestNode -> m a)
    -> m a
withTestNode net str f =
    runStdoutLoggingT $
    withSystemTempDirectory ("haskoin-node-test-" <> str <> "-") $ \w -> do
        node_inbox <- newInbox
        let cfg =
                NodeConfig
                    { nodeConfMaxPeers = 20
                    , nodeConfPeers = [("127.0.0.1", 9052)]
                    , nodeConfDiscover = True
                    , nodeConfNetAddr = NetworkAddress 0 (SockAddrInet 0 0)
                    , nodeConfNet = net
                    , nodeConfEvents = (`sendSTM` node_inbox)
                    , nodeConfTimeout = 10
                    }
        withNode cfg $ \(mgr, ch) ->
            lift $
            f TestNode {testMgr = mgr, testChain = ch, nodeEvents = node_inbox}
