module Ergvein.Types.Currency (
    Currency(..)
  , allCurrencies
  , Coin(..)
  , allCoins
  , coinCurrency
  , coinName
  , coinGenesisTime
  , coinBlockTime
  , coinBehind
  , isBitcoin
  , isErgo
  , isTestingCoin
  , btcResolution
  , ergoResolution
  , currencyResolution
  , currencyResolutionUnit
  , currencyName
  , currencyBlockDuration
  , btcSymbolUnit
  , ergoSymbolUnit
  , symbolUnit
  , MoneyUnit
  , Money(..)
  , moneyToRational
  , moneyToRationalUnit
  , moneyFromRational
  , moneyFromRationalUnit
  , showMoney
  , showMoneyUnit
  , UnitBTC(..)
  , defUnitBTC
  , allUnitsBTC
  , UnitERGO(..)
  , defUnitERGO
  , allUnitsERGO
  , Units(..)
  , defUnits
  , getUnitBTC
  , getUnitERGO
  , Fiat(..)
  , allFiats
  , curprefix
  ) where

import Data.Flat
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Aeson
import Text.Printf
import Web.HttpApiData

import qualified Data.Text as T

-- | Currency without tracking of its type
data Currency = Bitcoin | Ergo
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Serialize)
$(deriveJSON aesonOptions ''Currency)

instance ToJSONKey Currency where
instance FromJSONKey Currency where

instance FromHttpApiData Currency where
  parseUrlPiece t = case (T.toLower . T.strip) t of
    "bitcoin" -> Right Bitcoin
    "ergo" -> Right Ergo
    _ -> Left $ "Unknown Currency: " <> t

instance ToHttpApiData Currency where
  toQueryParam = T.toLower . T.pack . show

-- | All supported currencies
allCurrencies :: [Currency]
allCurrencies = [minBound .. maxBound]

-- | Supported currencies coins. Coin is currency ticker with respect to kinds
-- of networks of the currency.
data Coin =
    BTC
  | TBTC -- ^ Testnet Bitcoin
  | RTBTC -- ^ Regtest Bitcoin
  | ERGO
  | TERGO -- ^ Testnet Ergo
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Serialize)
$(deriveJSON aesonOptions ''Coin)

instance ToJSONKey Coin where
instance FromJSONKey Coin where

instance FromHttpApiData Coin where
  parseUrlPiece t = case (T.toLower . T.strip) t of
    "btc" -> Right BTC
    "tbtc" -> Right TBTC
    "rtbtc" -> Right RTBTC
    "ergo" -> Right ERGO
    "tergo" -> Right TERGO
    _ -> Left $ "Unknown Coin: " <> t

instance ToHttpApiData Coin where
  toQueryParam = T.toLower . T.pack . show

-- | All supported currencies
allCoins :: [Coin]
allCoins = [minBound .. maxBound]

-- | Drop network infromation from coin, get it kind
coinCurrency :: Coin -> Currency
coinCurrency c = case c of
  BTC -> Bitcoin
  TBTC -> Bitcoin
  RTBTC -> Bitcoin
  ERGO -> Ergo
  TERGO -> Ergo

-- | Is the currency is kind of Bitcoin
isBitcoin :: Coin -> Bool
isBitcoin cur = coinCurrency cur == Bitcoin

-- | Is the currency is kind of Ergo
isErgo :: Coin -> Bool
isErgo cur = coinCurrency cur == Ergo

-- | Is given currency is for testing purpose only
isTestingCoin :: Coin -> Bool
isTestingCoin cur = case cur of
  TBTC -> True
  RTBTC -> True
  TERGO -> True
  _ -> False

-- | Supported fiat
data Fiat = USD | EUR | RUB
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Flat, Serialize)
$(deriveJSON aesonOptions ''Fiat)

instance ToJSONKey Fiat where
instance FromJSONKey Fiat where

instance FromHttpApiData Fiat where
  parseUrlPiece t = case (T.toLower . T.strip) t of
    "usd" -> Right USD
    "eur" -> Right EUR
    "rub" -> Right RUB
    _ -> Left $ "Unknown Fiat: " <> t

instance ToHttpApiData Fiat where
  toQueryParam = T.toLower . T.pack . show

-- | All supported currencies
allFiats :: [Fiat]
allFiats = [minBound .. maxBound]

-- | Display units for BTC
data UnitBTC
  = BtcWhole
  | BtcMilli
  | BtcSat
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitBTC)
instance ToJSONKey UnitBTC where
instance FromJSONKey UnitBTC where

defUnitBTC :: UnitBTC
defUnitBTC = BtcWhole

allUnitsBTC :: [UnitBTC]
allUnitsBTC = [minBound .. maxBound]

btcResolution :: UnitBTC -> Int
btcResolution u = case u of
  BtcWhole -> 8
  BtcMilli -> 5
  BtcSat   -> 0
{-# INLINE btcResolution #-}

btcSymbolUnit :: UnitBTC -> Text
btcSymbolUnit u = case u of
  BtcWhole    -> "BTC"
  BtcMilli    -> "mBTC"
  BtcSat      -> "sat"
{-# INLINE btcSymbolUnit #-}

-- | Display units for ERGO
data UnitERGO
  = ErgWhole
  | ErgMilli
  | ErgNano
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions ''UnitERGO)
instance ToJSONKey UnitERGO where
instance FromJSONKey UnitERGO where

defUnitERGO :: UnitERGO
defUnitERGO = ErgWhole

allUnitsERGO :: [UnitERGO]
allUnitsERGO = [minBound .. maxBound]

ergoResolution :: UnitERGO -> Int
ergoResolution u = case u of
  ErgWhole -> 9
  ErgMilli -> 6
  ErgNano  -> 0
{-# INLINE ergoResolution #-}

ergoSymbolUnit :: UnitERGO -> Text
ergoSymbolUnit u = case u of
  ErgWhole    -> "ERG"
  ErgMilli    -> "mERG"
  ErgNano     -> "nERG"
{-# INLINE ergoSymbolUnit #-}

-- | Union units
data Units = Units {
    unitBTC   :: Maybe UnitBTC
  , unitERGO  :: Maybe UnitERGO
  } deriving (Eq, Ord, Show, Read, Generic)

$(deriveJSON aesonOptions ''Units)
instance ToJSONKey Units where
instance FromJSONKey Units where

defUnits :: Units
defUnits = Units {
    unitBTC   = Just BtcWhole
  , unitERGO  = Just ErgWhole
  }

getUnitBTC :: Units -> UnitBTC
getUnitBTC Units{..} = fromMaybe defUnitBTC unitBTC

getUnitERGO :: Units -> UnitERGO
getUnitERGO Units{..} = fromMaybe defUnitERGO unitERGO

-- | Amount of digits after point for currency
currencyResolution :: Currency -> Int
currencyResolution c = currencyResolutionUnit c defUnits
{-# INLINE currencyResolution #-}

currencyResolutionUnit :: Currency -> Units -> Int
currencyResolutionUnit c Units{..} = case c of
  Bitcoin -> btcResolution $ fromMaybe defUnitBTC unitBTC
  Ergo    -> ergoResolution $ fromMaybe defUnitERGO unitERGO
{-# INLINE currencyResolutionUnit #-}

symbolUnit :: Currency -> Units -> Text
symbolUnit cur Units{..} = case cur of
  Bitcoin -> btcSymbolUnit $ fromMaybe defUnitBTC unitBTC
  Ergo    -> ergoSymbolUnit $ fromMaybe defUnitERGO unitERGO

currencyName :: Currency -> Text
currencyName c = case c of
  Bitcoin -> "Bitcoin"
  Ergo -> "Ergo"
{-# INLINE currencyName #-}

coinName :: Coin -> Text
coinName c = case c of
  BTC -> "Bitcoin"
  TBTC -> "Testnet Bitcoin"
  RTBTC -> "Regtest Bitcoin"
  ERGO -> "Ergo"
  TERGO -> "Testnet Ergo"
{-# INLINE coinName #-}

-- | Get time of genesis block of coin
coinGenesisTime :: Coin -> UTCTime
coinGenesisTime c = case c of
  BTC -> fromEpoch (1231006505 :: Int)
  TBTC -> fromEpoch (1296699402 :: Int)
  RTBTC -> fromEpoch (1296699402 :: Int)
  ERGO -> fromEpoch (1561998777 :: Int)
  TERGO -> fromEpoch (1561998777 :: Int)
  where
    fromEpoch = posixSecondsToUTCTime . fromIntegral

-- | Average duration between blocks
currencyBlockDuration :: Currency -> NominalDiffTime
currencyBlockDuration c = case c of
  Bitcoin -> fromIntegral (600 :: Int)
  Ergo -> fromIntegral (120 :: Int)

-- | Approx time of block
coinBlockTime :: Coin -> Int -> UTCTime
coinBlockTime c i = addUTCTime (fromIntegral i * currencyBlockDuration (coinCurrency c)) $ coinGenesisTime c

-- | Get approx time we are behind the head
coinBehind :: Coin -> Int -> Int -> NominalDiffTime
coinBehind c n total = fromIntegral (total - n) * currencyBlockDuration (coinCurrency c)

-- | Smallest amount of currency
type MoneyUnit = Word64

-- | Amount of money tagged with specific currency
data Money = Money {
    moneyCoin     :: !Coin
  , moneyAmount   :: !MoneyUnit
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Convert to rational number amount of cryptocurrency
moneyToRational :: Money -> Rational
moneyToRational (Money c amount) = fromIntegral amount % (10 ^ currencyResolution (coinCurrency c))
{-# INLINE moneyToRational #-}

moneyToRationalUnit :: Money -> Units -> Rational
moneyToRationalUnit (Money c amount) units = fromIntegral amount % (10 ^ currencyResolutionUnit (coinCurrency c) units)
{-# INLINE moneyToRationalUnit #-}

-- | Convert a rational number to money value
moneyFromRational :: Coin -> Rational -> Money
moneyFromRational c amount = Money c val
  where
    val = fromIntegral (round $ amount * (10 ^ currencyResolution (coinCurrency c)) :: Int)
{-# INLINE moneyFromRational #-}

moneyFromRationalUnit :: Coin -> Units -> Rational -> Money
moneyFromRationalUnit c units amount = Money c val
  where
    val = fromIntegral (round $ amount * (10 ^ currencyResolutionUnit (coinCurrency c) units) :: Int)
{-# INLINE moneyFromRationalUnit #-}

-- | Print amount of cryptocurrency
showMoney :: Money -> Text
showMoney m = T.pack $ printf "%f" (realToFrac (moneyToRational m) :: Double)

showMoneyUnit :: Money -> Units -> Text
showMoneyUnit m units = T.pack $ printf "%f" (realToFrac (moneyToRationalUnit m units) :: Double)

curprefix :: Currency -> Text
curprefix cur = case cur of
  Bitcoin ->  "bitcoin://"
  Ergo    ->  "ergo://"
