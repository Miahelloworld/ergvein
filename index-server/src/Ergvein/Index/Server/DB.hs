{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  , withOpenDb
  ) where

import Conduit
import Control.Monad
import Control.Monad.Catch
import Data.Default
import Database.LevelDB.Base
import System.Directory

import Ergvein.Index.Server.DB.Monad

import qualified Ergvein.Index.Server.DB.Schema.Filters as DBF
import qualified Ergvein.Index.Server.DB.Schema.Indexer as DBI

data MyException = DbVersionMismatch
    deriving Show

instance Exception MyException

withOpenDb :: (MonadIO m, MonadMask m) => Bool -> DBTag -> FilePath -> (DB -> m a) -> m a
withOpenDb overwriteDbVerOnMismatch dbtag dbDirectory cont = do
  canonicalPathDirectory <- liftIO $ canonicalizePath dbDirectory
  dbStatePresent <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless dbStatePresent $ createDirectory canonicalPathDirectory
  withDB canonicalPathDirectory def{createIfMissing = True } $ \db -> do
    if overwriteDbVerOnMismatch || not dbStatePresent then do
      put db def schemaVersionRecKey schemaVersion
    else do
      dbSchemaVersion <- get db def schemaVersionRecKey
      unless (dbSchemaVersion == Just schemaVersion) $
        liftIO $ throwM DbVersionMismatch
    cont db
  where
    (schemaVersionRecKey, schemaVersion) = case dbtag of
      DBFilters -> (DBF.schemaVersionRecKey, DBF.schemaVersion)
      DBIndexer -> (DBI.schemaVersionRecKey, DBI.schemaVersion)
