module Ergvein.Faucet.Backend(
    ergveinFaucetApp
  ) where

import Data.Proxy
import Network.HTTP.Types (status404)
import Network.HTTP.Types.Status (ok200)
import Network.Wai
import Network.Wai.Middleware.Static
import Servant.API
import Servant.API.Generic
import Servant.Server
import Ergvein.Faucet.API
import Ergvein.Faucet.Backend.Config
import Ergvein.Faucet.Backend.Index
import Ergvein.Faucet.Backend.Monad
import Ergvein.Faucet.Backend.Server

import qualified Data.Text as T

statics :: FilePath -> Middleware
statics dir = staticPolicy $ addBase dir

staticsApp :: Config -> Server Raw
staticsApp Config{..} = Tagged $ midleware $ \_ sendResponse -> sendResponse $
  responseLBS status404 [("Content-Type", "text/plain")] "File not found"
  where
    midleware = case cfgStatics of
      Nothing -> id
      Just dir -> statics dir

type XenoFaucetAPI' = ToServantApi XenoFaucetAPI
type FullApi = XenoFaucetAPI' :<|> HttpAPI

ergveinFaucetApp :: Env -> Application
ergveinFaucetApp env = frontendMiddleware servantApp
  where
  Config{..} = envConfig env

  -- Manual response with frontend blob
  frontendMiddleware :: Middleware
  frontendMiddleware oldApp req doResp = case pathInfo req of
    (firstPart:_) | T.isPrefixOf "main_" firstPart && T.isSuffixOf ".js" firstPart -> do
      let respFile headers path = doResp $ responseFile ok200 headers path Nothing
      case cfgMainpageBlob of
        Just blobPath -> respFile [("Content-Type", "application/javascript"), ("Content-Encoding", "gzip")] blobPath
        Nothing -> case cfgStatics of
          Nothing -> doResp $ responseLBS status404 [("Content-Type", "text/plain")] "File not found"
          Just dir -> respFile [("Content-Type", "application/javascript")] $ dir <> "/main.js"
    _ -> oldApp req doResp

  fullApi = Proxy :: Proxy (FullApi :<|> ("static" :> Raw))
  api = Proxy :: Proxy FullApi
  servantApp = serve fullApi $ hoistServer api (runServerM env) fullServer :<|> staticsApp (envConfig env)
  fullServer = toServant ergveinFaucetServer :<|> httpServer
