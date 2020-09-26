{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Ergvein.Wallet.Worker.IndexerNetworkRefreshWorker
  ( indexerNetworkRefreshWorker
  , getAll
  ) where

import Control.Monad.Reader
import Control.Monad.Zip
import Data.Bifunctor
import Data.Maybe
import Data.Time
import Data.List
import Reflex.ExternalRef
import Data.Either

import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Index.Protocol.Types
import Network.DNS.Lookup
import Network.DNS.Types
import Network.DNS.Resolver
import Network.Socket
import Data.IP

import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Word

import qualified Data.List          as L
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

indexersCountE :: MonadIndexClient t m =>  m (Event t Int)
indexersCountE = do
  indexersD <- externalRefDynamic =<< getActiveConnsRef
  pure $ updated $ length . Map.elems <$> indexersD

dnsList :: [Domain]
dnsList = ["seed.cypra.io"]

getDNS :: [Domain] -> IO (Maybe [Text])
getDNS domains = findMMaybe f domains
  where
    f :: Domain -> IO (Maybe [Text])
    f x = do
      r <- resolve x
      pure $ if length r < 2 then Nothing else Just r
    resolve :: Domain -> IO [Text]
    resolve domain = do
      rs <- makeResolvSeed defaultResolvConf
      withResolver rs $ \r -> do
        v4 <- lookupA r domain
        v6 <- lookupAAAA r domain
        pure $ concat $ rights [(fmap showt <$> v4), (fmap showt <$> v6)]

    tran4 :: IPv4 -> SockAddr
    tran4 v4 = let 
      [a] = fromIntegral <$> fromIPv4 v4
      in SockAddrInet 8667 a

    tran6 :: IPv6 -> SockAddr
    tran6 v6 = let 
      [a,b,c,d] = fromIntegral <$> fromIPv6 v6
      in SockAddrInet6 8667 0 (a, b, c, d) 0
    
    findMapMMaybe :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
    findMapMMaybe f (x:xs) = do
      r <- f x
      if isJust r then
        pure r
      else
        findMMaybe f xs
    findMMaybe f [] = pure Nothing

getAll :: MonadIndexClient t m =>  m (Event t PeerResponse)
getAll = do
  indexersD <- externalRefDynamic =<< getActiveConnsRef
  ix <- readExternalRef =<< getActiveConnsRef
  let l  = Map.toList ix
  let s = length l
  let x = s - if s > 2 then ceiling $ fromIntegral s / 2 else s
  let z = indexConAddr <$> (sortBy (\x-> compare (indexConnEstablishedAt x) . indexConnEstablishedAt ) (snd <$> l))
  pure $ switchDyn $ leftmost . fmap (fmapMaybe onlyPeerResponse . indexConRespE) . Map.elems <$> indexersD
  where
    onlyPeerResponse = \case
      MPeerResponse peerResponse -> Just peerResponse
      _ -> Nothing

indexerNetworkRefreshWorker :: MonadFront t m => m ()
indexerNetworkRefreshWorker = do
  buildE            <- getPostBuild
  te                <- void <$> tickLossyFromPostBuildTime infoWorkerInterval
  activeUrlsRef     <- getActiveConnsRef

  let goE = leftmost [void te, buildE]
  void <$> activateURLList =<< (performEvent $ ffor goE $ const $ do
    ix <- readExternalRef activeUrlsRef
    let l  = Map.toList ix
        s = length l
        x = s - if s > 2 then ceiling $ fromIntegral s / 2 else s
        z = indexConAddr <$> (sortBy (\x-> compare (indexConnEstablishedAt x) . indexConnEstablishedAt ) (snd <$> l))
    pure z)

  --broadcastIndexerMessage $ (IndexerMsg $ MPeerRequest PeerRequest) <$ goE

{-}
import Control.Monad.Reader
import Control.Monad.Zip
import Data.Bifunctor
import Data.Maybe
import Data.Time
import Reflex.ExternalRef
import Servant.Client
import System.Random.Shuffle

import Ergvein.Index.API.Types
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Transaction
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.List          as L
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

minIndexers :: Int
minIndexers = 2

newIndexers :: (PlatformNatives, MonadIO m, HasClientManager m) => Set BaseUrl -> m (Set BaseUrl)
newIndexers knownIndexers = do
  mng <- getClientManager
  successfulResponses <- concat <$> ((`runReaderT` mng) $ mapM knownIndexersFrom $ Set.toList knownIndexers)
  let validIndexerUrls = Set.fromList $ catMaybes $ parseBaseUrl <$> successfulResponses
  pure validIndexerUrls
  where
    knownIndexersFrom url = do
      result <- getKnownPeersEndpoint url $ KnownPeersReq False
      case result of
        Right (KnownPeersResp list) -> pure list
        Left err -> do
          logWrite $ "[IndexersNetworkActualization][Getting peer list][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
          pure mempty

indexersNetwork :: forall m . (PlatformNatives, MonadIO m, HasClientManager m) => Int -> [BaseUrl] -> m (Map BaseUrl IndexerInfo, Set BaseUrl)
indexersNetwork targetAmount peers =
  go peers mempty mempty
  where
    go :: [BaseUrl] -> Map BaseUrl IndexerInfo -> Set BaseUrl -> m (Map BaseUrl IndexerInfo, Set BaseUrl)
    go toExplore exploredInfoMap result
      | length result == targetAmount || null toExplore =
        pure (exploredInfoMap, result)
      | otherwise = do
        let needed = targetAmount - length result
            available = length toExplore
            (indexers, toExplore') = splitAt (min needed available) toExplore

        newExploredInfoMap <- indexersInfo indexers

        let exploredInfoMap' = exploredInfoMap `Map.union` newExploredInfoMap
            newWorkingIndexers = Set.filter (`Map.member` exploredInfoMap') $ Set.fromList indexers
            median = medianScanInfoMap $ indInfoHeights <$> Map.elems exploredInfoMap'
            result' = Set.filter (matchMedian median . indInfoHeights . (exploredInfoMap' Map.!)) $ result `Set.union` newWorkingIndexers

        go toExplore' exploredInfoMap' result'

    matchMedian :: PeerScanInfoMap -> PeerScanInfoMap -> Bool
    matchMedian peer median = all (\currency -> predicate (peer Map.! currency) (median Map.! currency)) $ Map.keys peer
      where
        predicate (peerScannedHeight, peerActualHeight) (medianScannedHeight, medianActualHeight) =
          peerScannedHeight >= medianScannedHeight && peerActualHeight == medianActualHeight

    medianScanInfoMap :: [PeerScanInfoMap] -> PeerScanInfoMap
    medianScanInfoMap infos = let
      in bimap median' median' . munzip <$> Map.unionsWith (<>) (fmap pure <$> infos)
      where
        median' :: (Ord a) => [a] -> a
        median' v = L.sort v !! (length v `div` 2)

    indexersInfo :: [BaseUrl] -> m (Map BaseUrl IndexerInfo)
    indexersInfo urls = do
      mng <- getClientManager
      fmap mconcat $ (`runReaderT` mng) $ mapM peerInfo urls
      where
        peerInfo url = do
          t0 <- liftIO $ getCurrentTime
          result <- getInfoEndpoint url ()
          t1 <- liftIO $ getCurrentTime
          case result of
            Right info -> do
              let pingTime = diffUTCTime t1 t0
                  scanInfo = mconcat $ mapping <$> infoScanProgress info
              pure $ Map.singleton url $ IndexerInfo scanInfo pingTime
            Left err ->  do
              logWrite $ "[IndexersNetworkActualization][Getting info][" <> T.pack (showBaseUrl url) <> "]: " <> showt err
              pure mempty
        mapping :: ScanProgressItem -> PeerScanInfoMap
        mapping (ScanProgressItem currency scanned actual) = Map.singleton currency (scanned, actual)

indexersNetworkActualizationWorker :: MonadFront t m => m ()
indexersNetworkActualizationWorker = do
  buildE            <- getPostBuild
  refreshE          <- fst  <$> getIndexerInfoEF
  te                <- void <$> tickLossyFromPostBuildTime infoWorkerInterval
  settingsRef       <- getSettingsRef
  activeUrlsRef     <- getActiveUrlsRef
  inactiveUrlsRef   <- getInactiveAddrsRef
  archivedUrlsRef   <- getArchivedAddrsRef

  let goE = leftmost [void te, refreshE, buildE]

  performFork_ $ ffor goE $ const $ do
    inactiveUrls          <- readExternalRef inactiveUrlsRef
    archivedUrls          <- readExternalRef archivedUrlsRef
    settings              <- readExternalRef settingsRef
    currentNetworkInfoMap <- readExternalRef activeUrlsRef

    let maxIndexersToExplore = settingsActUrlNum settings
        indexersToExclude = inactiveUrls `Set.union` archivedUrls
        currentNetwork = Map.keysSet currentNetworkInfoMap

    fetchedIndexers <- newIndexers currentNetwork

    let filteredIndexers = currentNetwork `Set.union` fetchedIndexers Set.\\ indexersToExclude

    shuffledIndexers <- liftIO $ shuffleM $ Set.toList filteredIndexers
    (newNetworkInfoMap, newNetwork) <- indexersNetwork maxIndexersToExplore shuffledIndexers

    let resultingNetwork = if length newNetwork >= minIndexers then newNetwork else currentNetwork
        resultingNetworkInfoMap = Map.fromSet (newNetworkInfoMap Map.!?) resultingNetwork

    modifyExternalRefMaybe_ activeUrlsRef (\previous ->
      if previous /= resultingNetworkInfoMap then
        Just resultingNetworkInfoMap
      else Nothing)
-}
