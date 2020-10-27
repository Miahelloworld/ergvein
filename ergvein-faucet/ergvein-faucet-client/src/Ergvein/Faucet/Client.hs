module Ergvein.Faucet.Client(
    ClientError(..)
  , fromClientError
  , ClientConfig(..)
  , ClientM
  , runClientM
  , getBalance
  , mineBlocks
  , getHeight
  , sendToAddress
  , makeClientConfig
  , setBlockInterval
  , getBlockInterval
  , getTxConfirmations
  ) where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Proxy
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS    (tlsManagerSettings)
import Servant.API.Generic
import UnliftIO.Exception
import Ergvein.Faucet.API

import qualified Data.Text as T
import qualified Servant.Client as C

-- | Errors in 'ClientM' monad
data ClientError = InvalidBaseUrl !Text | ClientError !C.ClientError
  deriving (Eq, Show, Generic)

instance Exception ClientError

-- | Tighten an error info into servant client common client error type
fromClientError :: ClientError -> C.ClientError
fromClientError e@(InvalidBaseUrl{}) = C.ConnectionError (SomeException e)
fromClientError (ClientError e) = e

-- | Initial environment that is needed to execute 'ClientM' action
data ClientConfig = ClientConfig {
    clientUrl      :: !Text -- ^ Base url for API server
  , clientManager  :: !Manager -- ^ Connection manager
  } deriving (Generic)

-- | Monad that is used to perform calls to NEM NIS API.
newtype ClientM a = ClientM { unClientM :: ReaderT ClientConfig C.ClientM a }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadReader ClientConfig)

-- | Execute API actions
runClientM :: MonadIO m
  => ClientM a -- ^ Action
  -> ClientConfig -- ^ Required info to execute action
  -> m (Either ClientError a)
runClientM ma cfg@ClientConfig{..} = case C.parseBaseUrl . T.unpack $ clientUrl of
  Left e -> return $ Left . InvalidBaseUrl . T.pack . show $ e
  Right burl -> do
    res <- liftIO $ C.runClientM (runReaderT (unClientM ma) cfg) $ C.ClientEnv clientManager burl Nothing
    return $ first ClientError $ res

-- | Helper to embed 'ClientM' action into more specific monad
liftClientM :: C.ClientM a -> ClientM a
liftClientM = ClientM . lift

data AsClient
instance GenericMode AsClient where
  type AsClient :- api = C.Client C.ClientM api

api :: XenoFaucetAPI AsClient
api = fromServant $ C.client (Proxy :: Proxy (ToServant XenoFaucetAPI AsApi))

getBalance :: Currency -> ClientM Double
getBalance = liftClientM . getBalanceEndpoint api

mineBlocks :: Currency ->  Word -> ClientM ()
mineBlocks c = liftClientM . mineBlocksEndpoint api c

getHeight :: Currency -> ClientM Word
getHeight = liftClientM . getHeightEndpoint api

sendToAddress :: Currency -> SendToAddress -> ClientM Text
sendToAddress c = liftClientM . sendToAddressEndpoint api c

setBlockInterval :: Currency -> Maybe NominalDiffTime -> ClientM ()
setBlockInterval c = liftClientM . setBlockIntervalEndpoint api c

getBlockInterval :: Currency -> ClientM (Maybe NominalDiffTime)
getBlockInterval = liftClientM . getBlockIntervalEndpoint api

getTxConfirmations :: Currency -> Text -> ClientM (Maybe Int)
getTxConfirmations c tid = liftClientM $ getTxConfirmationsEndpoint api c (Just tid)

makeClientConfig :: MonadIO m => Text -> m ClientConfig
makeClientConfig url = do
  mng <- liftIO $ newManager tlsManagerSettings
  pure $ ClientConfig url mng
