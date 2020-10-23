{-# LANGUAGE MultiWayIf #-}
module Ergvein.Types.Keys (
    XPrvKey(..)
  , XPubKey(..)
  , EgvRootXPrvKey(..)
  , EgvRootXPubKey(..)
  , EgvXPrvKey(..)
  , EgvXPubKey(..)
  , EgvPubKeyBox(..)
  , PrvKeystore(..)
  , PubKeystore(..)
  , KeyPurpose(..)
  , ScanKeyBox(..)
  , xPrvExport
  , xPrvImport
  , xPubExport
  , xPubImport
  , getLastUnusedKey
  , getPublicKeys
  , egvXPubCurrency
  , egvXPubCoin
  , egvXPubNetworkType
  , egvXPubNetwork
  , getExternalPubKeyIndex
  , egvXPubKey
  , egvXPubLabel
  , setEgvXPubLabel
  , unEgvXPrvKey
  , egvXPubKeyAddress
  , xPubToBtcAddr
  , xPubToErgAddr
  , extractAddrs
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types       (Parser)
import Data.Serialize         (get, put)
import Data.Serialize.Get     (Get, getWord32be, getWord8, runGet)
import Data.Serialize.Put     (Putter, putWord32be, putWord8, runPut)
import Data.Text              (Text)
import Data.Vector            (Vector)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Crypto.Util
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Types.Transaction
import Text.Read              (readMaybe)

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize        as SE

-- | Parse a binary extended private key.
getXPrvKey :: EgvNetwork -> Get XPrvKey
getXPrvKey (EgvBtcNetwork net) = do
  ver <- getWord32be
  unless (ver == getExtSecretPrefix net) $ fail
      "Get: Invalid version for extended private key"
  XPrvKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> getPadPrvKey
getXPrvKey (EgvErgNetwork net) = do
  ver <- getWord32be
  unless (ver == getErgExtSecretPrefix net) $ fail
      "Get: Invalid version for extended private key"
  XPrvKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> getPadPrvKey

-- | Serialize an extended private key.
putXPrvKey :: EgvNetwork -> Putter XPrvKey
putXPrvKey (EgvBtcNetwork net) k = do
  putWord32be  $ getExtSecretPrefix net
  putWord8     $ xPrvDepth k
  putWord32be  $ xPrvParent k
  putWord32be  $ xPrvIndex k
  put          $ xPrvChain k
  putPadPrvKey $ xPrvKey k
putXPrvKey (EgvErgNetwork net) k = do
  putWord32be  $ getErgExtSecretPrefix net
  putWord8     $ xPrvDepth k
  putWord32be  $ xPrvParent k
  putWord32be  $ xPrvIndex k
  put          $ xPrvChain k
  putPadPrvKey $ xPrvKey k

-- | Parse a binary extended public key.
getXPubKey :: EgvNetwork -> Get XPubKey
getXPubKey (EgvBtcNetwork net) = do
  ver <- getWord32be
  unless (ver == getExtPubKeyPrefix net) $ fail
      "Get: Invalid version for extended public key"
  XPubKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> (pubKeyPoint <$> get)
getXPubKey (EgvErgNetwork net) = do
  ver <- getWord32be
  unless (ver == getErgExtPubKeyPrefix net) $ fail
      "Get: Invalid version for extended public key"
  XPubKey <$> getWord8
          <*> getWord32be
          <*> getWord32be
          <*> get
          <*> (pubKeyPoint <$> get)

-- | Serialize an extended public key.
putXPubKey :: EgvNetwork -> Putter XPubKey
putXPubKey (EgvBtcNetwork net) k = do
  putWord32be $ getExtPubKeyPrefix net
  putWord8    $ xPubDepth k
  putWord32be $ xPubParent k
  putWord32be $ xPubIndex k
  put         $ xPubChain k
  put         $ wrapPubKey True (xPubKey k)
putXPubKey (EgvErgNetwork net) k = do
  putWord32be $ getErgExtPubKeyPrefix net
  putWord8    $ xPubDepth k
  putWord32be $ xPubParent k
  putWord32be $ xPubIndex k
  put         $ xPubChain k
  put         $ wrapPubKey True (xPubKey k)

-- | Exports an extended private key to the BIP32 key export format ('Base58').
xPrvExport :: EgvNetwork -> XPrvKey -> Base58
xPrvExport n@(EgvBtcNetwork {}) = encodeBase58CheckBtc . runPut . putXPrvKey n
xPrvExport n@(EgvErgNetwork {}) = encodeBase58CheckErg . runPut . putXPrvKey n

-- | Exports an extended public key to the BIP32 key export format ('Base58').
xPubExport :: EgvNetwork -> XPubKey -> Base58
xPubExport n@(EgvBtcNetwork {}) = encodeBase58CheckBtc . runPut . putXPubKey n
xPubExport n@(EgvErgNetwork {}) = encodeBase58CheckErg . runPut . putXPubKey n

-- | Decodes a BIP32 encoded extended private key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPrvImport :: EgvNetwork -> Base58 -> Maybe XPrvKey
xPrvImport n@(EgvBtcNetwork {}) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckBtc
xPrvImport n@(EgvErgNetwork {}) = eitherToMaybe . runGet (getXPrvKey n) <=< decodeBase58CheckErg

-- | Decodes a BIP32 encoded extended public key. This function will fail if
-- invalid base 58 characters are detected or if the checksum fails.
xPubImport :: EgvNetwork -> Base58 -> Maybe XPubKey
xPubImport n@(EgvBtcNetwork {}) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckBtc
xPubImport n@(EgvErgNetwork {}) = eitherToMaybe . runGet (getXPubKey n) <=< decodeBase58CheckErg

-- | Wrapper for a root extended private key (a key without assigned network)
newtype EgvRootXPrvKey = EgvRootXPrvKey {unEgvRootXPrvKey :: XPrvKey}
  deriving (Eq, Show, Read)

instance ToJSON EgvRootXPrvKey where
  toJSON (EgvRootXPrvKey XPrvKey{..}) = object [
      "depth"  .= toJSON xPrvDepth
    , "parent" .= toJSON xPrvParent
    , "index"  .= toJSON xPrvIndex
    , "chain"  .= show xPrvChain
    , "key"    .= show xPrvKey
    ]

instance FromJSON EgvRootXPrvKey where
  parseJSON = withObject "EgvRootXPrvKey" $ \o -> do
    depth  <- o .: "depth"
    parent <- o .: "parent"
    index  <- o .: "index"
    chain  <- o .: "chain"
    key    <- o .: "key"
    case (readMaybe chain, readMaybe key) of
      (Just chain', Just key') -> pure $ EgvRootXPrvKey $ XPrvKey depth parent index chain' key'
      _ -> fail "failed to read chain code or key"

-- | Wrapper for a root extended public key (a key without assigned network)
newtype EgvRootXPubKey = EgvRootXPubKey {unEgvRootXPubKey :: XPubKey}
  deriving (Eq, Show)

instance ToJSON EgvRootXPubKey where
  toJSON (EgvRootXPubKey XPubKey{..}) = object [
      "depth"  .= toJSON xPubDepth
    , "parent" .= toJSON xPubParent
    , "index"  .= toJSON xPubIndex
    , "chain"  .= show xPubChain
    , "key"    .= show xPubKey
    ]

instance FromJSON EgvRootXPubKey where
  parseJSON = withObject "EgvRootXPubKey" $ \o -> do
    depth  <- o .: "depth"
    parent <- o .: "parent"
    index  <- o .: "index"
    chain  <- o .: "chain"
    key    <- o .: "key"
    case (readMaybe chain, readMaybe key) of
      (Just chain', Just key') -> pure $ EgvRootXPubKey $ XPubKey depth parent index chain' key'
      _ -> fail "failed to read chain code or key"

-- | Wrapper around XPrvKey for easy to/from json manipulations
data EgvXPrvKey =
    BtcXPrvKey { btcXPrvKey :: !XPrvKey, btcXPrvNetwork :: !NetworkType }
  | ErgXPrvKey { ergXPrvKey :: !XPrvKey, ergXPrvNetwork :: !NetworkType }
  deriving (Eq, Show)

unEgvXPrvKey :: EgvXPrvKey -> XPrvKey
unEgvXPrvKey key = case key of
  BtcXPrvKey k _ -> k
  ErgXPrvKey k _ -> k

egvXPrvNetwork :: EgvXPrvKey -> EgvNetwork
egvXPrvNetwork key = case key of
  BtcXPrvKey _ net -> EgvBtcNetwork $ bitcoinNetwork net
  ErgXPrvKey _ net -> EgvErgNetwork $ ergoNetwork net

egvXPrvNetworkType :: EgvXPrvKey -> NetworkType
egvXPrvNetworkType key = case key of
  BtcXPrvKey _ net -> net
  ErgXPrvKey _ net -> net

-- | Get JSON 'Value' from 'XPrvKey'.
xPrvToJSON :: EgvNetwork -> XPrvKey -> Value
xPrvToJSON net = String . xPrvExport net

-- | Decode an extended private key from a JSON string
xPrvFromJSON :: EgvNetwork -> Value -> Parser XPrvKey
xPrvFromJSON net =
    withText "xprv" $ \t ->
        case xPrvImport net t of
            Nothing -> fail "could not read xprv"
            Just x  -> return x

instance ToJSON EgvXPrvKey where
  toJSON k = case k of
    BtcXPrvKey key _ -> object [
        "currency" .= toJSON Bitcoin
      , "network"  .= toJSON (egvXPrvNetworkType k)
      , "prvKey"   .= xPrvToJSON (egvXPrvNetwork k) key
      ]
    ErgXPrvKey key _ -> object [
        "currency" .= toJSON Ergo
      , "network"  .= toJSON (egvXPrvNetworkType $ k)
      , "prvKey"   .= xPrvToJSON (egvXPrvNetwork k) key
      ]

instance FromJSON EgvXPrvKey where
  parseJSON = withObject "EgvXPrvKey" $ \o -> do
    currency <- o .: "currency"
    network <- o .: "network"
    key <- xPrvFromJSON (currencyNetwork currency network) =<< (o .: "prvKey")
    pure $ case currency of
      Bitcoin -> BtcXPrvKey key network
      Ergo -> ErgXPrvKey key network

-- | Wrapper around XPubKey for easy to/from json manipulations
data EgvXPubKey =
    ErgXPubKey {
      ergXPubKey     :: !XPubKey
    , ergXPubLabel   :: !Text
    , ergXPubNetwork :: !NetworkType
    }
  | BtcXPubKey {
      btcXPubKey     :: !XPubKey
    , btcXPubLabel   :: !Text
    , btcXPubNetwork :: !NetworkType
    }
  deriving (Eq, Show)

egvXPubCurrency :: EgvXPubKey -> Currency
egvXPubCurrency val = case val of
  ErgXPubKey{} -> Ergo
  BtcXPubKey{} -> Bitcoin

egvXPubCoin :: EgvXPubKey -> Coin
egvXPubCoin val = coinByNetwork (egvXPubCurrency val) (egvXPubNetworkType val)

egvXPubNetworkType :: EgvXPubKey -> NetworkType
egvXPubNetworkType val = case val of
  ErgXPubKey{..} -> ergXPubNetwork
  BtcXPubKey{..} -> btcXPubNetwork

egvXPubNetwork :: EgvXPubKey -> EgvNetwork
egvXPubNetwork val = currencyNetwork (egvXPubCurrency val) (egvXPubNetworkType val)

egvXPubKey :: EgvXPubKey -> XPubKey
egvXPubKey key = case key of
  ErgXPubKey k _ _ -> k
  BtcXPubKey k _ _ -> k

egvXPubLabel :: EgvXPubKey -> Text
egvXPubLabel key = case key of
  ErgXPubKey _ l _ -> l
  BtcXPubKey _ l _ -> l

setEgvXPubLabel :: Text -> EgvXPubKey -> EgvXPubKey
setEgvXPubLabel l key = case key of
  ErgXPubKey k _ net -> ErgXPubKey k l net
  BtcXPubKey k _ net -> BtcXPubKey k l net

xPubToBtcAddr :: XPubKey -> BtcAddress
xPubToBtcAddr key = pubKeyWitnessAddr $ wrapPubKey True (xPubKey key)

xPubToErgAddr :: XPubKey -> ErgAddress
xPubToErgAddr key = pubKeyErgAddr $ wrapPubKey True (xPubKey key)

pubKeyErgAddr :: PubKeyI -> ErgAddress
pubKeyErgAddr = ErgPubKeyAddress . VLAddr . BSS.toShort . SE.encode

egvXPubKeyAddress :: EgvXPubKey -> EgvAddress
egvXPubKeyAddress key = case key of
  ErgXPubKey k _ net -> ErgAddress (xPubToErgAddr k) net
  BtcXPubKey k _ net -> BtcAddress (xPubToBtcAddr k) net

-- | Get JSON 'Value' from 'XPubKey'.
xPubToJSON :: EgvNetwork -> XPubKey -> Value
xPubToJSON net = String . xPubExport net

-- | Decode an extended public key from a JSON string
xPubFromJSON :: EgvNetwork -> Value -> Parser XPubKey
xPubFromJSON net =
    withText "xpub" $ \t ->
        case xPubImport net t of
            Nothing -> fail "could not read xpub"
            Just x  -> return x

instance ToJSON EgvXPubKey where
  toJSON val = object [
      "currency"  .= toJSON cur
    , "network"   .= toJSON (egvXPubNetworkType val)
    , "pubKey"    .= xPubToJSON (egvXPubNetwork val) key
    , "label"     .= toJSON label
    ]
    where
      (cur, key, label) =  case val of
        ErgXPubKey k l _ -> (ERGO, k, l)
        BtcXPubKey k l _ -> (BTC, k, l)

instance FromJSON EgvXPubKey where
  parseJSON = withObject "EgvXPubKey" $ \o -> do
    currency <- o .: "currency"
    network <- o .: "network"
    key <- xPubFromJSON (currencyNetwork currency network) =<< (o .: "pubKey")
    label <- o .:? "label" .!= ""
    pure $ case currency of
      Bitcoin -> BtcXPubKey key label network
      Ergo    -> ErgXPubKey key label network

instance Ord EgvXPubKey where
  compare key1 key2 = case compare c1 c2 of
    EQ -> compare (xPubExport (currencyNetwork c1 (egvXPubNetworkType key1)) k1) (xPubExport (currencyNetwork c2 (egvXPubNetworkType key2)) k2)
    x -> x
    where
      (c1, k1, _) =  case key1 of
        ErgXPubKey k l _ -> (Ergo, k, l)
        BtcXPubKey k l _ -> (Bitcoin, k, l)
      (c2, k2, _) =  case key2 of
        ErgXPubKey k l _ -> (Ergo, k, l)
        BtcXPubKey k l _ -> (Bitcoin, k, l)

data PrvKeystore = PrvKeystore {
  prvKeystore'master   :: !EgvXPrvKey
  -- ^Extended private key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, prvKeystore'external :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 external extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, prvKeystore'internal :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 internal extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show)

$(deriveJSON aesonOptionsStripToApostroph ''PrvKeystore)

data EgvPubKeyBox = EgvPubKeyBox {
  pubKeyBox'key    :: !EgvXPubKey
, pubKeyBox'txs    :: !(S.Set TxId)
, pubKeyBox'manual :: !Bool
} deriving (Eq, Show)

$(deriveJSON aesonOptionsStripToApostroph ''EgvPubKeyBox)

data PubKeystore = PubKeystore {
  pubKeystore'master   :: !EgvXPubKey
  -- ^Extended public key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, pubKeystore'external :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 external extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, pubKeystore'internal :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 internal extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show)

$(deriveJSON aesonOptionsStripToApostroph ''PubKeystore)

getLastUnusedKey :: KeyPurpose -> PubKeystore -> Maybe (Int, EgvPubKeyBox)
getLastUnusedKey kp PubKeystore{..} = go Nothing vec
  where
    vec = case kp of
      Internal -> pubKeystore'internal
      External -> pubKeystore'external
    go :: Maybe (Int, EgvPubKeyBox) -> Vector EgvPubKeyBox -> Maybe (Int, EgvPubKeyBox)
    go mk v = if V.null v then mk else let
      kb@(EgvPubKeyBox _ txs m) = V.last v
      in if m || not (S.null txs)
        then mk
        else go (Just (V.length v - 1, kb)) $ V.init v

data ScanKeyBox = ScanKeyBox {
  scanBox'key     :: !EgvXPubKey
, scanBox'purpose :: !KeyPurpose
, scanBox'index   :: !Int
} deriving (Show)

-- | Get all public keys in storage (external and internal) to scan for new transactions for them.
getPublicKeys :: PubKeystore -> Vector ScanKeyBox
getPublicKeys PubKeystore{..} = ext <> int
  where
    ext = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) External i) pubKeystore'external
    int = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) Internal i) pubKeystore'internal

getExternalPubKeyIndex :: PubKeystore -> Int
getExternalPubKeyIndex = V.length . pubKeystore'external

-- | Extract addresses from keystore
extractAddrs :: PubKeystore -> Vector EgvAddress
extractAddrs pks = fmap (egvXPubKeyAddress . scanBox'key) $ getPublicKeys pks

-- | Supported key purposes. It represents /change/ field in BIP44 derivation path.
-- External chain is used for addresses that are meant to be visible outside of the wallet (e.g. for receiving payments).
-- Internal chain is used for addresses which are not meant to be visible outside of the wallet and is used for return transaction change.
data KeyPurpose = External | Internal
  deriving (Eq, Ord, Show, Read)
$(deriveJSON defaultOptions ''KeyPurpose)
