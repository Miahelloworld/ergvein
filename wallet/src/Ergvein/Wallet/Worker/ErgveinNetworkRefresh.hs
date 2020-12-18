
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Ergvein.Wallet.Worker.ErgveinNetworkRefresh
  ( ergveinNetworkRefresh
  , initNetwork
  ) where

import Control.Monad.IO.Class
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Either.Combinators
import Data.Maybe
import Data.Time.Clock
import Network.DNS
import Network.Socket
import Reflex.ExternalRef
import System.Random.Shuffle

import Ergvein.Index.Protocol.Types
import Ergvein.Text
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Monad.Prim
import Control.Lens
import Control.Lens.Fold
import Data.Map.Lens

import qualified Data.Vector            as V
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Ergvein.Types.Currency as C

minOperableAmount, targetAmount :: Int
minOperableAmount = 1
targetAmount = 16

workerDelay :: NominalDiffTime
workerDelay = 4

ergveinNetworkRefresh ::(MonadFront t m, MonadIO m, MonadIndexClient t m, MonadHasSettings t m, PlatformNatives) => m ()
ergveinNetworkRefresh = do
  dnsSettingsD <- fmap _settingsDns <$> getSettingsD
  timerE <- void <$> tickLossyFromPostBuildTime workerDelay
  buildE <- getPostBuild
  settD <- fmap _settingsDiscoveryEnabled <$> getSettingsD
  let goE = gate (current settD) $ leftmost [timerE, buildE]
  activeUrlsRef <- getActiveConnsRef
  gE <- performEvent $ ffor goE $ \_-> do
    act <- readExternalRef activeUrlsRef
    pure $ settingsAddrs %~ Map.filterWithKey (\k a -> _peerInfoIsPinned a || has (at k . _Just) act)

  gE' <- updateSettingsAsync gE
  activeUrlsE <- performEvent $ ffor gE' $ const $ readExternalRef activeUrlsRef

  let activePeerAmountE  = length . Map.toList <$> activeUrlsE
      notOperablePeerAmountE  = void $ ffilter (< minOperableAmount) activePeerAmountE 
      insufficientPeerAmountE = void $ ffilter (< targetAmount) activePeerAmountE
  restoreFromDNS notOperablePeerAmountE
  fetchNewPeer insufficientPeerAmountE

initNetwork :: (MonadIndexClient t m, MonadHasSettings t m) => m ()
initNetwork  = do
  settingsRef <- getSettingsRef
  buildE <- getPostBuild
  activeUrlsE <- performEvent $ ffor buildE $ const $ readExternalRef settingsRef
  let activeUrlsE' = void $ ffilter (null . _settingsAddrs)  activeUrlsE
  restoreFromDNS activeUrlsE'

restoreFromDNS :: (MonadIndexClient t m, MonadHasSettings t m) => Event t () -> m ()
restoreFromDNS e = do
  dnsSettingsD <- fmap _settingsDns <$> getSettingsD
  reloadedFromSeedE <- performEvent $ ffor e $ const $ do
    dns <- sample $ current dnsSettingsD
    rs <- liftIO $ resolveSeed $ Set.toList dns 
    newSet <- liftIO $ getDNS rs seedList
    pure $ fromMaybe defaultIndexers newSet

  void $ addManyDiscovered reloadedFromSeedE

fetchNewPeer :: MonadFront t m => Event t () -> m ()
fetchNewPeer e = do
  let reqE = (C.BTC, MPeerRequest PeerRequest) <$ e
  respE <- requestRandomIndexer reqE
  let nonEmptyAddressesE = fforMaybe respE $ \(_, msg) -> case msg of
        MPeerResponse PeerResponse {..} | not $ V.null peerResponseAddresses -> Just peerResponseAddresses
        _-> Nothing
  newIndexerE <- performEvent $ ffor nonEmptyAddressesE $ \addrs ->
    liftIO $ convertA . head <$> (shuffleM $ V.toList addrs)
  void $ addDiscovered newIndexerE

convertA Address{..} = case addressType of
    IPV4 -> let
      port = (fromInteger $ toInteger addressPort)
      ip  =  fromRight (error "address") $ parseOnly anyWord32be addressAddress
      addr = SockAddrInet port ip
      in showt addr
    IPV6 -> let
      port = (fromInteger $ toInteger addressPort)
      ip  =  fromRight (error "address") $ parseOnly ((,,,) <$> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be) addressAddress
      addr = SockAddrInet6 port 0 ip 0
      in showt addr