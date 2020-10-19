module Ergvein.Faucet.API(
    PostJson
  , GetJson
  , BodyJson
  , XenoFaucetAPI(..)
  , SendToAddress(..)
  , UserAgent
  , UserAgentHeader
  , Currency(..)
  ) where

import Data.Text (Text)
import Data.Time
import Servant.API
import Servant.API.Generic
import Ergvein.Faucet.Aeson

import qualified Data.Text as T

-- | Supported currencies
data Currency = BTC | ERGO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
$(deriveJSON aesonOptions ''Currency)

instance ToJSONKey Currency where
instance FromJSONKey Currency where

instance FromHttpApiData Currency where
  parseUrlPiece t = case (T.toLower . T.strip) t of
    "btc" -> Right BTC
    "ergo" -> Right ERGO
    _ -> Left $ "Unknown Currency: " <> t

instance ToHttpApiData Currency where
  toQueryParam = T.toLower . T.pack . show

-- | Wrapped user agent string
newtype UserAgent = UserAgent { unUserAgent :: Text }
  deriving (Eq, Show, ToHttpApiData, FromHttpApiData)

-- | Header with user agent
type UserAgentHeader = Header "Custom-User-Agent" UserAgent

-- | Helper to shorten post with json body
type PostJson a = Post '[JSON] a

-- | Helper to shorten get with json body
type GetJson a = Get '[JSON] a

-- | Helper to shorten body in json format
type BodyJson a = ReqBody '[JSON] a

-- | Main API of faucet
data XenoFaucetAPI route = XenoFaucetAPI {
  getBalanceEndpoint :: route :-
    "balance" :> Capture "currency" Currency :> GetJson Double
, mineBlocksEndpoint :: route :-
    "mine" :> Capture "currency" Currency :> BodyJson Word :> PostJson ()
, getHeightEndpoint :: route :-
    "height" :> Capture "currency" Currency :> GetJson Word
, sendToAddressEndpoint :: route :-
    "sendtoaddress" :> Capture "currency" Currency :> BodyJson SendToAddress :> PostJson Text
, setBlockIntervalEndpoint :: route :-
    "blockinterval" :> Capture "currency" Currency :> BodyJson (Maybe NominalDiffTime) :> PostJson ()
, getBlockIntervalEndpoint :: route :-
    "blockinterval" :> Capture "currency" Currency :> GetJson (Maybe NominalDiffTime)
, getTxConfirmationsEndpoint :: route :-
    "tx" :> "confirmations" :> Capture "currency" Currency :> QueryParam "tid" Text :> GetJson (Maybe Int)
} deriving (Generic)

-- | Body of endpoint `sendToAddressEndpoint`
data SendToAddress = SendToAddress {
  sendToAddressAddress :: !Text
, sendToAddressAmount  :: !Double
} deriving (Generic)

deriveJSON (aesonOptionsStripPrefix "sendTo") ''SendToAddress
