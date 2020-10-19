module Ergvein.IO(
    periodic
  , sleep
  , toMicroseconds
  , readFileSafe
  , readLazyByteStringSafe
  , readStrictByteStringSafe
) where

import Control.Exception
import Control.Concurrent.Thread.Delay
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import System.Directory

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T

{-# NOINLINE periodic #-}
periodic :: MonadIO m => NominalDiffTime -> m () -> m ()
periodic time proc = fix $ \next -> proc >> sleep time >> next

-- | Stop the thread for some time in seconds.
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep dt = liftIO . delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000

readFileSafe :: FilePath -> IO (Maybe Text)
readFileSafe file = do
  isOk <- doesFileExist file
  if isOk
    then fmap Just $ T.readFile file
    else return Nothing

readLazyByteStringSafe :: FilePath -> IO (Maybe LB.ByteString)
readLazyByteStringSafe file = do
  catchIOError (fmap Just $ LB.readFile file) (const $ return Nothing)

readStrictByteStringSafe :: FilePath -> IO (Maybe BS.ByteString)
readStrictByteStringSafe file =
  catchIOError (fmap Just $ BS.readFile file) (const $ return Nothing)

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = Control.Exception.catch
