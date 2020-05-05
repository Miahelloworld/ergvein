module Ergvein.Index.Server.DB.Queries where

import Conduit
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Conversion
import Data.Proxy
import Data.Word
import Database.Esqueleto
import Database.Esqueleto.Pagination
import Safe 

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Data.Time.Clock

import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import qualified Database.Persist as DT

pageLoadSize :: PageSize
pageLoadSize = PageSize 65536

pagedEntitiesStream ::(PersistRecordBackend record backend, PersistQueryRead backend, PersistUniqueRead backend,
                      BackendCompatible SqlBackend backend, BackendCompatible SqlBackend (BaseBackend backend),
                      Ord typ, PersistField typ, MonadIO m) 
                      => EntityField record typ -> ConduitT a [Entity record] (ReaderT backend m) ()
pagedEntitiesStream entityField = let
  pagedStream = streamEntities emptyQuery entityField pageLoadSize Ascend (Range Nothing Nothing)
  in pagedStream .| (CL.chunksOf $ unPageSize pageLoadSize)

getScannedHeight :: MonadIO m => Currency -> QueryT m (Maybe (Entity ScannedHeightRec))
getScannedHeight currency = fmap headMay $ select $ from $ \scannedHeight -> do
  where_ (scannedHeight ^. ScannedHeightRecCurrency ==. val currency)
  pure scannedHeight

upsertScannedHeight :: MonadIO m => Currency -> Word64 -> QueryT m (Entity ScannedHeightRec)
upsertScannedHeight currency h = upsert (ScannedHeightRec currency h) [ScannedHeightRecHeight DT.=. h]

addNewPeer :: MonadIO m => NewPeer -> QueryT m ()
addNewPeer discoveredPeer = do
  currentTime <- liftIO getCurrentTime
  insert_ $ convert @_ @DiscoveredPeerRec (currentTime, discoveredPeer)

refreshPeerValidationTime :: MonadIO m => DiscoveredPeerRecId -> QueryT m ()
refreshPeerValidationTime peerId = do
  currentTime <- liftIO getCurrentTime
  update $ \peer -> do 
    where_ (peer ^. DiscoveredPeerRecId ==. val peerId)
    set peer [DiscoveredPeerRecLastValidatedAt =. val currentTime]

getNewPeers :: MonadIO m => QueryT m [Peer]
getNewPeers = fmap (convert @(Entity DiscoveredPeerRec)) <$> select (from pure)

insertBlock  :: MonadIO m  => BlockMetaInfo -> QueryT m (Key BlockMetaRec)
insertBlock block = insert $ convert block

rowsCount :: forall record m . (BackendCompatible SqlBackend (PersistEntityBackend record),
                                PersistEntity record, MonadIO m)
                                 => Proxy record -> QueryT m Word64
rowsCount _ = do 
  result <- select $ from (\(_ :: SqlExpr (Entity record)) -> pure $ countRows)
  pure $ unValue $ head $ result

chunksCount :: forall record m . (BackendCompatible SqlBackend (PersistEntityBackend record),
                                PersistEntity record, MonadIO m)
                                 => Proxy record -> QueryT m Word64
chunksCount _ = do 
  rCount <- rowsCount (Proxy :: Proxy record)
  let cCount = ceiling $ fromIntegral rCount / (fromIntegral $  unPageSize pageLoadSize)
  pure cCount