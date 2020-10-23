{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Settings (
    Settings(..)
  , loadSettings
  , storeSettings
  , defaultSettings
  , getSettingsExplorerUrl
  , defaultIndexers
  , defIndexerPort
  , defaultIndexerTimeout
  , ExplorerUrls
  , defaultExplorerUrls
  , defaultDns
  , SocksConf(..)
  , torSocks
  , toSocksProxy
  -- * Helpers
  , makeSockAddr
  , parseIP
  ) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (encodeFile)
import Data.Maybe
import Data.Text(Text, pack, unpack)
import Data.Time (NominalDiffTime)
import Data.Yaml (encodeFile)
import Network.Socket (HostName, PortNumber)
import System.Directory

import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Wallet.IP
import Ergvein.Wallet.Language
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Yaml(readYamlEither')

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Socks5 as S5
import qualified Data.Set as S

#ifdef ANDROID
import Android.HaskellActivity
import Ergvein.Wallet.Native
#endif

-- | Explorer urls are set per currency and network type
type ExplorerUrls = M.Map Currency (M.Map NetworkType Text)

defaultExplorerUrls ::ExplorerUrls
defaultExplorerUrls = [
    (Bitcoin, [
      (Mainnet, "https://www.blockchain.com/btc")
    , (Testnet, "https://www.blockchain.com/btc-testnet")
    ])
  , (Ergo, [
      (Mainnet, "https://explorer.ergoplatform.com/en/transactions")
    , (Testnet, "https://testnet.ergoplatform.com/en/transactions")
    ])
  ]

data SocksConf = SocksConf {
  socksConfAddr :: !IP
, socksConfPort :: !Int
} deriving (Eq, Show)

instance ToJSON SocksConf where
  toJSON SocksConf{..} = object [
      "address" .= showt socksConfAddr
    , "port" .= socksConfPort
    ]

instance FromJSON SocksConf where
  parseJSON = withObject "SocksConf" $ \o -> do
    addrText <- o .: "address"
    socksConfAddr <- maybe (fail "Cannot parse IP of socks proxy") pure . parseIP $ addrText
    socksConfPort <- o .: "port"
    pure SocksConf{..}

-- | Default tor socks proxy
torSocks :: SocksConf
torSocks = SocksConf "127.0.0.1" 9050

toSocksProxy :: SocksConf -> S5.SocksConf
toSocksProxy (SocksConf a p) = S5.defaultSocksConfFromSockAddr $ makeSockAddr a p

type IndexerUrls = M.Map NetworkType [Text]

defaultIndexers :: IndexerUrls
defaultIndexers = [(Mainnet, [
      "ergvein-indexermainnet1.hxr.team"
    , "ergvein-indexermainnet2.hxr.team"
    , "ergvein-indexermainnet3.hxr.team"
    , "indexer.ergvein.net"       -- OwO
    ])
  , (Testnet, [
      "testnet.ergvein.net"
    ])
  , (Regtest, [
      "127.0.0.1:19667"
    ])
  ]

data Settings = Settings {
  settingsLang              :: Language
, settingsStoreDir          :: Text
, settingsConfigPath        :: Text
, settingsUnits             :: Maybe Units
, settingsReqTimeout        :: NominalDiffTime
, settingsActiveAddrs       :: IndexerUrls
, settingsDeactivatedAddrs  :: IndexerUrls
, settingsArchivedAddrs     :: IndexerUrls
, settingsExplorerUrl       :: ExplorerUrls
, settingsPortfolio         :: Bool
, settingsFiatCurr          :: Fiat
, settingsDns               :: S.Set HostName
, settingsSocksProxy        :: Maybe SocksConf
, settingsNetwork           :: NetworkType
} deriving (Eq, Show)

makeLensesWith humbleFields ''Settings

$(deriveJSON defaultOptions ''PortNumber)
$(deriveJSON defaultOptions ''SockAddr)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o -> do
    settingsLang              <- o .: "lang"
    settingsStoreDir          <- o .: "storeDir"
    settingsConfigPath        <- o .: "configPath"
    settingsUnits             <- o .: "units"
    settingsReqTimeout        <- o .: "reqTimeout"
    settingsActiveAddrs       <- o .:? "activeAddrs" .!= defaultIndexers
    settingsDeactivatedAddrs  <- o .:? "deactivatedAddrs" .!= []
    settingsArchivedAddrs     <- o .:? "archivedAddrs" .!= []
    settingsExplorerUrl       <- o .:? "explorerUrl" .!= defaultExplorerUrls
    settingsPortfolio         <- o .:? "portfolio" .!= False
    settingsFiatCurr          <- o .:? "fiatCurr"  .!= USD
    mdns                      <- o .:? "dns"
    settingsSocksProxy        <- o .:? "socksProxy"
    settingsNetwork           <- o .:? "network" .!= Mainnet
    let settingsDns = case fromMaybe [] mdns of
          [] -> defaultDns
          dns -> S.fromList dns
    pure Settings{..}

instance ToJSON Settings where
  toJSON Settings{..} = object [
      "lang"              .= toJSON settingsLang
    , "storeDir"          .= toJSON settingsStoreDir
    , "configPath"        .= toJSON settingsConfigPath
    , "units"             .= toJSON settingsUnits
    , "reqTimeout"        .= toJSON settingsReqTimeout
    , "activeAddrs"       .= toJSON settingsActiveAddrs
    , "deactivatedAddrs"  .= toJSON settingsDeactivatedAddrs
    , "archivedAddrs"     .= toJSON settingsArchivedAddrs
    , "explorerUrl"       .= toJSON settingsExplorerUrl
    , "portfolio"         .= toJSON settingsPortfolio
    , "fiatCurr"          .= toJSON settingsFiatCurr
    , "dns"               .= toJSON settingsDns
    , "socksProxy"        .= toJSON settingsSocksProxy
    , "network"           .= toJSON settingsNetwork
   ]

defIndexerPort :: NetworkType -> PortNumber
defIndexerPort nt = case nt of
  Mainnet -> 8667
  Testnet -> 18667
  Regtest -> 19667

defaultIndexerTimeout :: NominalDiffTime
defaultIndexerTimeout = 20

defaultDns :: S.Set HostName
defaultDns = S.fromList $ if isAndroid
  then ["8.8.8.8","8.8.4.4", "1.1.1.1"]
  else [] -- use resolv.conf

defaultSettings :: FilePath -> Settings
defaultSettings home =
  let storePath   = home <> "/store"
      configPath  = home <> "/config.yaml"
  in Settings {
        settingsLang              = English
      , settingsStoreDir          = pack storePath
      , settingsConfigPath        = pack configPath
      , settingsUnits             = Just defUnits
      , settingsReqTimeout        = defaultIndexerTimeout
      , settingsExplorerUrl       = defaultExplorerUrls
      , settingsPortfolio         = False
      , settingsFiatCurr          = USD
      , settingsActiveAddrs       = defaultIndexers
      , settingsDeactivatedAddrs  = []
      , settingsArchivedAddrs     = []
      , settingsDns               = defaultDns
      , settingsSocksProxy        = Nothing
      , settingsNetwork           = Mainnet
      }

getSettingsExplorerUrl :: Currency -> NetworkType -> Settings -> Text
getSettingsExplorerUrl cur net settings = fromMaybe defaultUrl $ M.lookup net =<< M.lookup cur (settingsExplorerUrl settings)
  where
    defaultUrl = fromMaybe "getSettingsExplorerUrl: not-defined-url" $ M.lookup net =<< M.lookup cur defaultExplorerUrls

-- | TODO: Implement some checks to see if the configPath folder is ok to write to
storeSettings :: MonadIO m => Settings -> m ()
storeSettings s = liftIO $ do
  let configPath = settingsConfigPath s
  createDirectoryIfMissing True $ unpack $ T.dropEnd 1 $ fst $ T.breakOnEnd "/" configPath
  encodeFile (unpack configPath) s

#ifdef ANDROID
loadSettings :: (MonadIO m, PlatformNatives) => Maybe FilePath -> m Settings
loadSettings = const $ liftIO $ do
  mpath <- getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let configPath = path <> "/config.yaml"
      ex <- doesFileExist configPath
      cfg <- if not ex
        then pure $ defaultSettings path
        else fmap (either (const $ defaultSettings path) id) $ readYamlEither' configPath
      createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
      encodeFile (unpack $ settingsConfigPath cfg) cfg
      pure cfg

#else
mkDefSettings :: MonadIO m => m Settings
mkDefSettings = liftIO $ do
  home <- getHomeDirectory
  putStrLn   "[ WARNING ]: Failed to load config. Reverting to default values: "
  putStrLn $ "Config path: " <> home <> "/.ergvein/config.yaml"
  putStrLn $ "Store  path: " <> home <> "/.ergvein/store"
  putStrLn $ "Language   : English"
  pure $ defaultSettings (home <> "/.ergvein")

loadSettings :: MonadIO m => Maybe FilePath -> m Settings
loadSettings mpath = liftIO $ case mpath of
  Nothing -> do
    home <- getHomeDirectory
    let path = home <> "/.ergvein/config.yaml"
    putStrLn "[ WARNING ]: No path provided. Trying the default: "
    putStrLn path
    loadSettings $ Just path
  Just path -> do
    ex <- doesFileExist path
    cfg <- if not ex
      then mkDefSettings
      else either (const mkDefSettings) pure =<< readYamlEither' path
    createDirectoryIfMissing True (unpack $ settingsStoreDir cfg)
    encodeFile (unpack $ settingsConfigPath cfg) cfg
    pure cfg
#endif
