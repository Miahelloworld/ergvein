module Ergvein.Types.Network (
    BtcNetwork
  , Network(..)
  , ErgNetwork(..)
  , EgvNetwork(..)
  , egvNetworkTag
  , btcNetworkTag
  , ergNetworkTag
  , parseNetworkTag
  , btc
  , btcTest
  , btcRegTest
  , erg
  , ergTest
  , coinNetwork
  , coinIndex
  , btcNetworkCoin
  , ergNetworkCoin
  , egvNetworkCoin
  , NetworkType(..)
  , encodeNetworkType
  , decodeNetworkType
  , coinByNetwork
  , coinNetworkType
  , allCoinsFor
  , bitcoinNetwork
  , ergoNetwork
  , currencyNetwork
  ) where

import Data.Flat
import Data.Text (Text)
import Data.Word
import Ergvein.Crypto.Keys
import Ergvein.Types.Currency
import Network.Haskoin.Constants
import Data.Aeson.Types

import qualified Data.Text as T

-- | Settings tag that indicates whether we in testing or production mode of blockchains
data NetworkType = Mainnet | Testnet | Regtest
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Encode tag into text
encodeNetworkType :: NetworkType -> Text
encodeNetworkType v = case v of
  Mainnet -> "mainnet"
  Testnet -> "testnet"
  Regtest -> "regtest"

-- | Decode tag from text
decodeNetworkType :: Text -> Maybe NetworkType
decodeNetworkType t = case T.toLower $ T.strip t of
  "mainnet" -> Just Mainnet
  "testnet" -> Just Testnet
  "regtest" -> Just Regtest
  _ -> Nothing

instance ToJSON NetworkType where
  toJSON = String . encodeNetworkType
  {-# INLINE toJSON #-}

instance FromJSON NetworkType where
  parseJSON = withText "NetworkType" $ \t ->
    maybe (fail $ "Unknown network tag " <> T.unpack t) pure $ decodeNetworkType t
  {-# INLINE parseJSON #-}

instance ToJSONKey NetworkType where
instance FromJSONKey NetworkType where

-- | Add network type info to currency to get ticker
coinByNetwork :: Currency -> NetworkType -> Coin
coinByNetwork c t = case c of
  Bitcoin -> case t of
    Mainnet -> BTC
    Testnet -> TBTC
    Regtest -> RTBTC
  Ergo -> case t of
    Mainnet -> ERGO
    Testnet -> TERGO
    Regtest -> TERGO
{-# INLINE coinByNetwork #-}

-- | Extract whether the coin is testnet or production network
coinNetworkType :: Coin -> NetworkType
coinNetworkType c = case c of
  BTC -> Mainnet
  TBTC -> Testnet
  RTBTC -> Regtest
  ERGO -> Mainnet
  TERGO -> Testnet
{-# INLINE coinNetworkType #-}

-- | Get list of coins for given network type
allCoinsFor :: NetworkType -> [Coin]
allCoinsFor nt = flip coinByNetwork nt <$> allCurrencies
{-# INLINE allCoinsFor #-}

bitcoinNetwork :: NetworkType -> BtcNetwork
bitcoinNetwork t = case t of
  Mainnet -> btc
  Testnet -> btcTest
  Regtest -> btcRegTest
{-# INLINE bitcoinNetwork #-}

ergoNetwork :: NetworkType -> ErgNetwork
ergoNetwork t = case t of
  Mainnet -> erg
  Testnet -> ergTest
  Regtest -> ergTest
{-# INLINE ergoNetwork #-}

currencyNetwork :: Currency -> NetworkType -> EgvNetwork
currencyNetwork c t = case c of
  Bitcoin -> EgvBtcNetwork $ bitcoinNetwork t
  Ergo -> EgvErgNetwork $ ergoNetwork t
{-# INLINE currencyNetwork #-}

type BtcNetwork = Network

data ErgNetwork = ErgNetwork
  { -- | lowercase alphanumeric and dashes
    getErgNetworkName              :: !String
    -- | network Haskell identifier
  , getErgNetworkIdent             :: !String
    -- | prefix for 'Base58' P2PK addresses
  , getErgAddrPrefix               :: !Word8
 -- | prefix for 'Base58' P2SH addresses
  , getErgScriptHashPrefix         :: !Word8
    -- | prefix for 'Base58' P2S addresses
  , getErgScriptPrefix             :: !Word8
    -- | prefix for WIF private key
  , getErgSecretPrefix             :: !Word8
    -- | prefix for extended public key
  , getErgExtPubKeyPrefix          :: !Word32
    -- | prefix for extended private key
  , getErgExtSecretPrefix          :: !Word32
    -- | BIP44 derivation path root
  , getErgBip44Coin                :: !Word32
  } deriving (Eq, Show, Generic)

data EgvNetwork
  = EgvBtcNetwork {getBtcNetwork :: BtcNetwork}
  | EgvErgNetwork {getErgNetwork :: ErgNetwork}
  deriving (Eq, Show, Generic)

egvNetworkTag :: EgvNetwork -> Text
egvNetworkTag (EgvBtcNetwork btcNet) = btcNetworkTag btcNet
egvNetworkTag (EgvErgNetwork ergNet) = ergNetworkTag ergNet

btcNetworkTag :: BtcNetwork -> Text
btcNetworkTag = T.pack . getNetworkName

ergNetworkTag :: ErgNetwork -> Text
ergNetworkTag = T.pack . getErgNetworkName

parseNetworkTag :: Text -> Maybe EgvNetwork
parseNetworkTag t = case (T.toLower . T.strip) t of
  "btc"     -> Just $ EgvBtcNetwork btc
  "btctest" -> Just $ EgvBtcNetwork btcTest
  "btcreg"  -> Just $ EgvBtcNetwork btcRegTest
  "erg"     -> Just $ EgvErgNetwork erg
  "ergtest" -> Just $ EgvErgNetwork ergTest
  _         -> Nothing

-- | Ergo network. Symbol: ERG.
erg :: ErgNetwork
erg =
    ErgNetwork
    { getErgNetworkName = "erg"
    , getErgNetworkIdent = "erg"
    , getErgAddrPrefix = 1
    , getErgScriptHashPrefix = 2
    , getErgScriptPrefix = 3
    , getErgSecretPrefix = 128
    , getErgExtPubKeyPrefix = 0x0488b21e
    , getErgExtSecretPrefix = 0x0488ade4
    , getErgBip44Coin = 429
    }

-- | Testnet for Ergo network.
ergTest :: ErgNetwork
ergTest =
    ErgNetwork
    { getErgNetworkName = "ergtest"
    , getErgNetworkIdent = "ergTest"
    , getErgAddrPrefix = 17
    , getErgScriptHashPrefix = 18
    , getErgScriptPrefix = 19
    , getErgSecretPrefix = 239
    , getErgExtPubKeyPrefix = 0x043587cf
    , getErgExtSecretPrefix = 0x04358394
    , getErgBip44Coin = 1
    }

-- | Get built in constants for currency
coinNetwork :: Coin -> EgvNetwork
coinNetwork t = case t of
  BTC   -> EgvBtcNetwork btc
  TBTC  -> EgvBtcNetwork btcTest
  RTBTC -> EgvBtcNetwork btcRegTest
  ERGO  -> EgvErgNetwork erg
  TERGO -> EgvErgNetwork ergTest

-- | Get bip44 index in derivation path for given currency. Testnet coins have
-- always 1 as index.
coinIndex :: Coin -> KeyIndex
coinIndex t = case t of
  BTC   -> getBip44Coin btc
  TBTC  -> 1
  RTBTC -> 1
  ERGO  -> getErgBip44Coin erg
  TERGO -> 1

btcNetworkCoin :: BtcNetwork -> Maybe Coin
btcNetworkCoin net = case btcNetworkTag net of
  "btc"     -> Just BTC
  "btctest" -> Just TBTC
  "btcreg"  -> Just RTBTC
  _         -> Nothing

ergNetworkCoin :: ErgNetwork -> Maybe Coin
ergNetworkCoin net = case ergNetworkTag net of
  "erg"     -> Just ERGO
  "ergtest" -> Just TERGO
  _         -> Nothing

egvNetworkCoin :: EgvNetwork -> Maybe Coin
egvNetworkCoin enet = case enet of
  EgvBtcNetwork net -> btcNetworkCoin net
  EgvErgNetwork net -> ergNetworkCoin net
