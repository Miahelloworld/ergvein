module Ergvein.Types.Address (
      BtcAddress
    , ErgAddress(..)
    , EgvAddress(..)
    , VLAddr(..)
    , egvAddrToString
    , egvAddrFromString
    , egvAddrCurrency
    , btcAddrToString'
    , btcAddrToString
  ) where

import Data.Aeson
import Data.Aeson.Types        (Parser)
import Data.ByteString.Short   (ShortByteString)
import Data.Hashable           (Hashable)
import Data.Serialize          (Serialize, put)
import Data.Serialize.Get      (Get, getWord8, runGet)
import Data.Serialize.Put      (Putter, putWord8, runPut)
import Data.String             (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text               (Text)
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Network
import GHC.Generics            (Generic)

import qualified Data.ByteString.Short     as BSS
import qualified Data.Serialize            as S
import qualified Data.Serialize.Get        as Get
import qualified Data.Serialize.Put        as Put
import qualified Network.Haskoin.Address   as HA
import qualified Text.Read                 as R

type BtcAddress = HA.Address

-- | Type for ERGO variable-length addresses.
newtype VLAddr = VLAddr { getErgVLAddr :: ShortByteString }
  deriving (Eq, Generic, Ord, Hashable)

instance Show VLAddr where
  showsPrec _ = shows . encodeHex . BSS.fromShort . getErgVLAddr

instance Read VLAddr where
  readPrec = do
    R.String str <- R.lexP
    maybe R.pfail return $ VLAddr . BSS.toShort <$> decodeHex (cs str)

instance IsString VLAddr where
  fromString str =
    case decodeHex $ cs str of
      Nothing -> error "Could not decode Ergo address from hex string"
      Just bs -> VLAddr (BSS.toShort bs)

instance Serialize VLAddr where
  put (VLAddr sbs) = Put.putShortByteString sbs
  get = VLAddr <$> (Get.remaining >>= Get.getShortByteString)

data ErgAddress
  = ErgPubKeyAddress     { getErgAddrAL      :: !VLAddr }
  | ErgScriptHashAddress { getErgAddrHash160 :: !Hash192 }
  | ErgScriptAddress     { getErgAddrAL      :: !VLAddr }
  deriving (Eq, Generic, Show, Read, Serialize)

data EgvAddress
  = BtcAddress { getBtcAddr :: !BtcAddress }
  | ErgAddress { getErgAddr :: !ErgAddress }
  deriving (Eq, Generic, Show, Read, Serialize)

-- | Binary serializer for 'Base58' ERGO addresses.
base58PutErg :: ErgNetwork -> Putter ErgAddress
base58PutErg net (ErgPubKeyAddress a) = do
  putWord8 (getErgAddrPrefix net)
  put a
base58PutErg net (ErgScriptHashAddress a) = do
  putWord8 (getErgScriptHashPrefix net)
  put a
base58PutErg net (ErgScriptAddress a) = do
  putWord8 (getErgScriptPrefix net)
  put a

-- | Deserializer for binary 'Base58' ERGO addresses.
base58GetErg :: ErgNetwork -> Get ErgAddress
base58GetErg net = do
    pfx <- getWord8
    f pfx
  where
    f x
      | x == getErgAddrPrefix       net = ErgPubKeyAddress     <$> S.get
      | x == getErgScriptHashPrefix net = ErgScriptHashAddress <$> S.get
      | x == getErgScriptPrefix     net = ErgScriptAddress     <$> S.get
      | otherwise = fail "Does not recognize address prefix"

btcAddrToString' :: BtcNetwork -> BtcAddress -> Text
btcAddrToString' net addr = case HA.addrToString net addr of
  Nothing -> undefined -- FIXME
  Just s -> s

btcAddrToString :: BtcAddress -> Text
btcAddrToString = btcAddrToString' net
  where net = getBtcNetwork $ getCurrencyNetwork BTC

ergAddrToString :: ErgAddress -> Text
ergAddrToString = encodeBase58CheckErg . runPut . base58PutErg net
  where net = getErgNetwork $ getCurrencyNetwork ERGO

egvAddrCurrency :: EgvAddress -> Currency
egvAddrCurrency addr = case addr of
  BtcAddress{} -> BTC
  ErgAddress{} -> ERGO

egvAddrToString :: EgvAddress -> Text
egvAddrToString (BtcAddress addr) = btcAddrToString addr
egvAddrToString (ErgAddress addr) = ergAddrToString addr

btcAddrFromString :: Text -> Maybe BtcAddress
btcAddrFromString = HA.stringToAddr net
  where net = getBtcNetwork $ getCurrencyNetwork BTC

ergAddrFromString :: Text -> Maybe ErgAddress
ergAddrFromString t = eitherToMaybe . runGet (base58GetErg net) =<< decodeBase58CheckErg t
  where net = getErgNetwork $ getCurrencyNetwork ERGO

egvAddrFromString :: Currency -> Text -> Maybe EgvAddress
egvAddrFromString BTC  addr = BtcAddress <$> btcAddrFromString addr
egvAddrFromString ERGO addr = ErgAddress <$> ergAddrFromString addr

egvAddrToJSON :: EgvAddress -> Value
egvAddrToJSON = String . egvAddrToString

egvAddrFromJSON :: Currency -> Value -> Parser EgvAddress
egvAddrFromJSON = \case
  BTC -> withText "address" $ \t ->
    case btcAddrFromString t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ BtcAddress x
  ERGO -> withText "address" $ \t ->
    case ergAddrFromString t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ ErgAddress x

instance ToJSON EgvAddress where
  toJSON egvAddr@(BtcAddress _) = object [
      "currency" .= toJSON BTC
    , "address"  .= egvAddrToJSON egvAddr
    ]
  toJSON egvAddr@(ErgAddress _) = object [
      "currency" .= toJSON ERGO
    , "address"  .= egvAddrToJSON egvAddr
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    cur  <- o .: "currency"
    egvAddrFromJSON cur =<< (o .: "address")
