module Ergvein.Types.Derive(
    deriveCurrencyMasterPrvKey
  , deriveCurrencyMasterPubKey
  , derivePrvKey
  , derivePubKey
  , DerivPrefix
  , defaultDerivPathPrefix
  , defaultDerivePath
  , parseDerivePath
  , showDerivPath
  , extendDerivPath
  ) where

import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Text.Read (readMaybe)

import qualified Data.Text             as T

-- | Shorthand for encoded BIP48 derivation path like m/84'/0'/0' (hard)
type DerivPrefix = [KeyIndex]

-- | Derivation path starts with 84 according to BIP84
defaultDerivPathPrefix :: DerivPrefix
defaultDerivPathPrefix = [84]

-- | Derivation path from BIP44 that compatible with BIP84
defaultDerivePath :: Coin -> DerivPrefix
defaultDerivePath coin = extendDerivPath coin defaultDerivPathPrefix

-- | Parse string m/0'/0'/0' as derivation path
parseDerivePath :: Text -> Maybe DerivPrefix
parseDerivePath s = do
  sm <- T.stripPrefix "m/" s
  ss <- traverse (T.stripSuffix "'") $ T.splitOn "/" sm
  traverse (readMaybe . T.unpack) ss

-- | Display derivation path as string m\/84'\/0'\/0'
showDerivPath :: DerivPrefix -> Text
showDerivPath ks = "m/" <> T.intercalate "/" (fmap ((<> "'") . showt) ks)

-- | Extend derivation path with c'\/0' if it contains only from purpose prefix
extendDerivPath :: Coin -> DerivPrefix -> DerivPrefix
extendDerivPath coin [a] = [a, coinIndex coin, 0]
extendDerivPath _ as = as

-- | Derive a BIP44 and BIP84 compatible private key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute private key with path /m\/84'\/c'\/0'/.
-- The overide derivation path is expected to be full derivation path
deriveCurrencyMasterPrvKey :: DerivPrefix -> EgvRootXPrvKey -> Coin -> EgvXPrvKey
deriveCurrencyMasterPrvKey hardPath rootPrvKey coin = case coinCurrency coin of
  Bitcoin -> BtcXPrvKey derivedPrvKey (coinNetworkType coin)
  Ergo -> ErgXPrvKey derivedPrvKey (coinNetworkType coin)
  where
    derivedPrvKey = foldl hardSubKey (unEgvRootXPrvKey rootPrvKey) hardPath

-- | Derive a BIP44 compatible public key for a specific currency.
-- Given a parent private key /m/
-- and a currency with code /c/, this function will compute public key with path /m\/84'\/c'\/0'/.
deriveCurrencyMasterPubKey :: DerivPrefix -> EgvRootXPrvKey -> Coin -> EgvXPubKey
deriveCurrencyMasterPubKey hardPath rootPrvKey coin = case coinCurrency coin of
  Bitcoin -> BtcXPubKey derivedPubKey "" (coinNetworkType coin)
  Ergo -> ErgXPubKey derivedPubKey "" (coinNetworkType coin)
  where
    derivedPrvKey = foldl hardSubKey (unEgvRootXPrvKey rootPrvKey) hardPath
    derivedPubKey = deriveXPubKey derivedPrvKey

-- | Derive a BIP44 compatible private key with a given purpose (external or internal) and index.
-- Given a parent private key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPrvKey' as the first argument of this function.
derivePrvKey :: EgvXPrvKey -> KeyPurpose -> KeyIndex -> EgvXPrvKey
derivePrvKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      mKey = unEgvXPrvKey masterKey
      derivedKey = foldl prvSubKey mKey path
  in case masterKey of
    BtcXPrvKey _ net -> BtcXPrvKey derivedKey net
    ErgXPrvKey _ net -> ErgXPrvKey derivedKey net

-- | Derive a BIP44 compatible public key with a given purpose (external or internal) and index.
-- Given a parent public key /m/, purpose /p/ and an index /i/, this function will compute /m\/p\/i/.
-- It is planned to use the result of 'deriveCurrencyMasterPubKey' as the first argument of this function.
derivePubKey :: EgvXPubKey -> KeyPurpose -> KeyIndex -> EgvXPubKey
derivePubKey masterKey keyPurpose index =
  let pCode = if keyPurpose == External then 0 else 1
      path = [pCode, index]
      derivedKey mk = foldl pubSubKey mk path
  in case masterKey of
    ErgXPubKey k _ net -> ErgXPubKey (derivedKey k) "" net
    BtcXPubKey k _ net -> BtcXPubKey (derivedKey k) "" net
