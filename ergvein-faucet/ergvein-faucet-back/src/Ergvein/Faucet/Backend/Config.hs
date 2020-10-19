module Ergvein.Faucet.Backend.Config(
    Config(..)
  , NodeConfig(..)
  , loadConfig
  , configMainJsPath
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Yaml.Config
import GHC.Generics
import Ergvein.Aeson

-- | Connection to BTC node in regtest mode
data NodeConfig = NodeConfig {
  nodeHost     :: !Text
, nodePort     :: !Int
, nodeUser     :: !Text
, nodePassword :: !Text
} deriving (Generic)

deriveJSON (aesonOptionsStripPrefix "node") ''NodeConfig

-- | Main server config
data Config = Config {
  cfgPort         :: !Int
, cfgHost         :: !Text
, cfgStatics      :: !(Maybe FilePath) -- ^ Folder with static HTML/CSS/JS to serve.
, cfgMainpageBlob :: !(Maybe FilePath) -- ^ You can specify direct path to main.js
, cfgBlockPeriod  :: !(Maybe NominalDiffTime) -- ^ Amount of time between automatic blocks
, cfgNode         :: !NodeConfig
} deriving (Generic)

deriveJSON (aesonOptionsStripPrefix "cfg") ''Config

-- | Load config from file
loadConfig :: MonadIO m => FilePath -> m Config
loadConfig path = liftIO $ loadYamlSettings [path] [] useEnv

-- | Get path to main.js blob
configMainJsPath :: Config -> FilePath
configMainJsPath Config{..} = case cfgMainpageBlob of
  Nothing -> fromMaybe "." cfgStatics <> "/main.js"
  Just v -> v
