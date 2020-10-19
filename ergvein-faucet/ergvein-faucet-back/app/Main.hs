module Main where

import Control.Monad.Logger
import Data.Semigroup ((<>))
import Data.String
import Network.Wai.Handler.Warp
import Options.Applicative
import Text.Read
import Ergvein.Faucet.Backend
import Ergvein.Faucet.Backend.Config
import Ergvein.Faucet.Backend.Monad

import qualified Data.Text as T

data Options = Options {
  configPath :: FilePath
}

options :: Parser Options
options = Options
  <$> strArgument
      (  metavar "CONFIG_PATH"
      <> help "Config path"
      <> value "./config.yaml"
      <> showDefault )

main :: IO ()
main = app =<< execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Debug faucet for BTC in regtest mode"
      <> header "ergvein-faucet - debug faucet for BTC in regtest mode" )

app :: Options -> IO ()
app Options{..} = do
  cfg@Config{..} <- loadConfig configPath
  putStrLn $ "Started listening on " ++ T.unpack cfgHost ++ ":" ++ show cfgPort
  let host = fromString . T.unpack $ cfgHost
  let settings = setPort cfgPort $ setHost host defaultSettings
  env <- runStdoutLoggingT $ newEnv cfg
  runSettings settings $ ergveinFaucetApp env
