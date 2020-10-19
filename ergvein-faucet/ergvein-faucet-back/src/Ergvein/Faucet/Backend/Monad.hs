module Ergvein.Faucet.Backend.Monad(
    Env(..)
  , newEnv
  , ServerM
  , AsServerM
  , runServerM
  , runServerMIO
  , getMainJsHash
  , getConfig
  , runBtcRpc
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Immortal.Worker
import Data.Foldable (traverse_)
import Data.IORef
import Data.Text (Text)
import Data.Time
import Ergvein.Faucet.Backend.Config
import Ergvein.IO
import Network.Bitcoin.Api.Client
import Servant.Server
import Servant.Server.Generic

import qualified Control.Immortal as Immortal
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Base32String.Default as B32
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.Bitcoin.Api.Mining as Btc
import qualified Network.Bitcoin.Api.Wallet as Btc

data Env = Env {
  envConfig            :: !Config
, envLogger            :: !(Chan (Loc, LogSource, LogLevel, LogStr))
, envMainJsHash        :: !Text
, envBlockIntervalChan :: !(Chan (Maybe NominalDiffTime))
, envBlockIntervalVar  :: !(IORef (Maybe NominalDiffTime))
}

-- | Calculate hash of file
hashOfFile :: (MonadIO m, MonadLogger m) => FilePath -> m Text
hashOfFile fname = do
  logInfoN $ "Calculating hash of file " <> T.pack fname
  cnt <- liftIO $ BS.readFile fname
  pure $ B32.toText . B32.fromBytes . SHA256.hash $ cnt

newEnv :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => Config -> m Env
newEnv cfg = do
  logger <- liftIO $ newChan
  void . liftIO . forkIO $ runStdoutLoggingT $ unChanLoggingT logger
  jsHash <- hashOfFile $ configMainJsPath cfg
  intChan <- liftIO newChan
  intVar <- liftIO $ newIORef $ cfgBlockPeriod cfg
  liftIO $ writeChan intChan $ cfgBlockPeriod cfg
  let env = Env {
          envConfig            = cfg
        , envLogger            = logger
        , envMainJsHash        = jsHash
        , envBlockIntervalChan = intChan
        , envBlockIntervalVar  = intVar
        }
  spawnBlockMiner env
  pure env

-- | For transforming records with endpoints into records of handlers
type AsServerM = AsServerT ServerM

newtype ServerM a = ServerM { unServerM :: ReaderT Env (LoggingT Handler) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadThrow, MonadCatch, MonadReader Env)

-- | Run server monad in servant handler (don't print logs from channel where they are accumulated)
runServerM :: Env -> ServerM a -> Handler a
runServerM e = runChanLoggingT (envLogger e) . flip runReaderT e . unServerM

-- | Execution of 'ServerM' in IO monad
runServerMIO :: Env -> ServerM a -> IO a
runServerMIO env m = do
  ea <- runHandler $ runServerM env m
  case ea of
    Left e -> fail $ "runServerMIO: " <> show e
    Right a -> return a

-- | Get hash of main js file
getMainJsHash :: MonadReader Env m => m Text
getMainJsHash = asks envMainJsHash

-- | Get config file from env
getConfig :: MonadReader Env m => m Config
getConfig = asks envConfig

-- | Run action with BTC node
runBtcRpc :: Text -> (Client -> IO a) -> ServerM a
runBtcRpc traceLabel ma = do
  logInfoN $ "Calling node " <> traceLabel
  cfg <- getConfig
  let NodeConfig{..} = cfgNode cfg
  liftIO $ withClient (T.unpack nodeHost) nodePort nodeUser nodePassword ma

-- | Periodic mine of new block
spawnBlockMiner :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => Env -> m ()
spawnBlockMiner env = do
  mineThreadRef <- liftIO $ newIORef Nothing
  void $ worker "block-miner-control" $ const $ forever $ do
    mdt <- liftIO $ readChan $ envBlockIntervalChan env
    mmineThread <- liftIO $ readIORef mineThreadRef
    liftIO $ traverse_ Immortal.stop mmineThread
    flip traverse_ mdt $ \dt -> do
      tid <- miner dt
      liftIO $ writeIORef mineThreadRef $ Just tid
  where
    miner dt = worker "block-miner" $ const $
      liftIO $ periodic dt $ runServerMIO env $ void $ runBtcRpc "miner" $ \c -> do
        addr <- Btc.newAddress c
        Btc.generateToAddress c 1 addr
