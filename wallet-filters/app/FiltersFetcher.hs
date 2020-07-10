module Main where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text (Text, unpack, pack, intercalate)
import Data.Text.Encoding
import Ergvein.Index.API.Types
import Ergvein.Index.Client.V1
import Ergvein.Text
import Ergvein.Types.Currency
import GHC.Generics
import Network.Bitcoin.Api.Client
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Generic
import Servant.Client.Core
import System.IO

import qualified Data.HashMap.Strict as HM
import qualified Data.HexString as HS
import qualified Data.Serialize as S
import qualified Network.Bitcoin.Api.Blockchain as API
import qualified Data.Text.IO as T

data Options = Options {
  indexerAddress  :: Maybe Text <?> "Address of indexer node"
, testnet         :: Bool <?> "Is this testnet network"
, firstBlock      :: Int <?> "From which block to fetch filters"
, lastBlock       :: Int <?> "To which block to fetch filters"
, outputFile      :: FilePath <?> "Where to write down resulted filters"
} deriving (Generic)

instance ParseRecord Options

getNodeAddress :: Options -> Text
getNodeAddress Options{..} = fromMaybe "127.0.0.1" $ unHelpful indexerAddress

nodeCall :: MonadIO m => Manager -> ReaderT Manager m (Either ClientError a) -> m a
nodeCall mng f = either (fail . show) pure =<< runReaderT f mng

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergvein transactions fetcher"
  mng <- liftIO newTlsManager
  burl <- parseBaseUrl $ unpack $ getNodeAddress opts
  withFile (unHelpful outputFile) WriteMode $ \h ->
    downloadLoop mng burl h 1000 (unHelpful firstBlock) (unHelpful lastBlock)

downloadLoop :: Manager -> BaseUrl -> Handle -> Int -> Int -> Int -> IO ()
downloadLoop mng burl h n i0 i1
  | i0 > i1 = pure ()
  | otherwise = do
    let n' = min n (i1 - i0)
    putStrLn $ "Getting " <> show i0  <> " ... " <> show (i0 + n')
    rsp <- nodeCall mng $ getBlockFiltersEndpoint burl BlockFiltersRequest {
        filtersReqCurrency = BTC
      , filtersReqStartHeight = fromIntegral i0
      , filtersReqAmount = fromIntegral n'
      }
    flip traverse_ (zip [0..] rsp) $ \(i,(bhash,fhxr)) -> T.hPutStrLn h $ intercalate "," [showt i, bhash, fhxr]
    downloadLoop mng burl h n (i0+n) i1

--   let blockNum = unHelpful nodeBlock
--   putStrLn $ "Getting block with height " <> show blockNum
--   hash <- nodeCall opts $ \c -> API.getBlockHash c blockNum
--   putStrLn $ "Block hash is" <> show hash
--   putStrLn "Getting block"
--   mblockRaw <- nodeCall opts $ \c -> API.getBlockRaw c hash
--   block <- maybe (fail "Cannot find block!") (pure . either error id . S.decode @Block . hex2bs . HS.toText) mblockRaw
--   let txIds = uniqTxs $ drop 1 $ blockInputTxs block -- dropping coinbase
--   putStrLn $ "Block downloaded. Block has " <> show (length txIds) <> " input transactions."
--   txs <- traverse (fetchTx opts) txIds
--   putStrLn $ "Writing to " <> unHelpful outputFile
--   withFile (unHelpful outputFile) WriteMode $ \h -> traverse_ (writeTx h) txs
--   putStrLn "Done"
--
-- fetchTx :: Options -> TxHash -> IO Tx
-- fetchTx opts h = do
--   putStrLn $ "Fetching tx " <> (unpack . txHashToHex) h
--   mtx <- nodeCall opts $ \c -> API.getRawTransaction c (HS.hexString . encodeUtf8 . txHashToHex $ h)
--   maybe (fail "Cannot find tx!") (pure . either error id . S.decode @Tx . hex2bs . HS.toText) mtx
--
-- writeTx :: Handle -> Tx -> IO ()
-- writeTx h tx = hPutStrLn h $ unpack . bs2Hex $ S.encode tx
