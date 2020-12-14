{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  , openDb
  ) where

import Conduit
import Control.Exception
import Control.Monad
import Data.Default
import Database.LevelDB.Base
import System.Directory

import Ergvein.Index.Server.DB.Monad

import qualified Ergvein.Index.Server.DB.Schema.Filters as DBF
import qualified Ergvein.Index.Server.DB.Schema.Indexer as DBI

data MyException = DbVersionMismatch
    deriving Show

instance Exception MyException

openDb :: (MonadIO m) => Bool -> DBTag -> FilePath -> m DB
openDb overwriteDbVerOnMismatch dbtag dbDirectory = do
  canonicalPathDirectory <- liftIO $ canonicalizePath dbDirectory
  dbStatePresent <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless dbStatePresent $ createDirectory canonicalPathDirectory
  db <- open canonicalPathDirectory def {createIfMissing = True }
  if overwriteDbVerOnMismatch || not dbStatePresent then do
    put db def schemaVersionRecKey schemaVersion
  else do
    dbSchemaVersion <- get db def schemaVersionRecKey
    unless (dbSchemaVersion == Just schemaVersion) $
      throw DbVersionMismatch
  pure db
  where
    (schemaVersionRecKey, schemaVersion) = case dbtag of
      DBFilters -> (DBF.schemaVersionRecKey, DBF.schemaVersion)
      DBIndexer -> (DBI.schemaVersionRecKey, DBI.schemaVersion)
