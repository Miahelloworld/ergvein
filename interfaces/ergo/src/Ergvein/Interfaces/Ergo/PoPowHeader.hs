{-|
The port of:
ergo/src/main/scala/org/ergoplatform/modifiers/history/PoPowHeader.scala
-}
module Ergvein.Interfaces.Ergo.PoPowHeader where

import Control.Arrow
import Control.Lens
import Data.Aeson as A
import Data.List
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Numeric.Natural
import qualified Data.ByteString as BS

import Ergvein.Aeson
import Ergvein.Interfaces.Ergo.Common.BigNat
import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Mining.Difficulty.RequiredDifficulty
import Ergvein.Interfaces.Ergo.Modifiers.History.PoPow
import Ergvein.Interfaces.Ergo.Modifiers.History.ValidateChain
import Ergvein.Interfaces.Ergo.Scorex.Util.Package
import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed
import qualified Ergvein.Interfaces.Ergo.Api as Api
import qualified Ergvein.Interfaces.Ergo.Mining.AutolykosSolution as Autolukos


data PoPowHeader = PoPowHeader {
  header :: !Header
, interlinks :: ![ModifierId]
} deriving (Eq, Show)

instance Serialize PoPowHeader where
    put obj = do
        -- val headerBytes = obj.header.bytes
        -- w.putInt(headerBytes.length)
        -- w.putBytes(headerBytes)
        put . VLQLengthPrefixed $ S.encode $ header obj
        -- w.putInt(obj.interlinks.size)
        -- obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
        put . VLQLengthPrefixed $ interlinks obj
    get = do
        -- val headerSize = r.getInt()
        -- val header = HeaderSerializer.parseBytes(r.getBytes(headerSize))
        VLQLengthPrefixed headerbs <- get
        header <- either fail pure $ S.decode headerbs
        -- val linksQty = r.getInt()
        -- val interlinks = (0 until linksQty).map(_ => bytesToId(r.getBytes(32)))
        VLQLengthPrefixed interlinks <- get
        pure PoPowHeader {..}

instance IsChainElem PoPowHeader where
  type BlockHash PoPowHeader = ModifierId
  blockHash = calculateHeaderId . header
  -- val level = log2(requiredTarget.doubleValue) - log2(realTarget.doubleValue)
  mu h = floor . logBase @Double 2 $ fromIntegral $ requiredTarget `div` actualTarget
    where
      requiredTarget = secp256k1_n `div` (unBigNat . unDifficulty . decodeCompactBits . nBits . header $ h)
      -- ^ https://github.com/ergoplatform/ergo/blob/cf687538d50970409c8bc6db3d0ddae35de548da/src/main/scala/org/ergoplatform/modifiers/history/popow/PoPowAlgos.scala#L97
      secp256k1_n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 :: Natural
      -- ^ https://github.com/bitcoin-core/secp256k1/blob/544435fc90a5672d862e2a51f44c10251893b97d/src/ecdsa_impl.h#L18
      actualTarget = unBigNat . Autolukos.distance . powSolution . header $ h

instance (HasNormaliseHeader a, IsChainElem a, Ord (BlockHash a)) => IsChain [a] where
  type Element [a] = a
  type Container [a] = []
  chainElems = id
  chainLength = length
  chainFromList = id
  findDivergingSubchains = findDivergingSubchainsWithList
  isValidChainAnchoredTo h hs = isValidChainAnchoredTo' (normalizeHeader h) (normalizeHeader <$> hs)

instance HasNormaliseHeader PoPowHeader where
  -- normalizeHeader :: PoPowHeader -> PoPowNormHeader
  normalizeHeader h@PoPowHeader {..} = PoPowNormHeader {
      blockId     = calculateHeaderId header
    , level       = mu h
    , header'     = header
    , interlinks' = interlinks
    }

cookPoPowHeader :: Api.FullBlock -> PoPowHeader
cookPoPowHeader b = PoPowHeader {
    header     = Api.headerFromApi . Api.header $ b
  , interlinks = unpackInterlinksFromFullBlock b
  }
  where
    unpackInterlinksFromFullBlock :: Api.FullBlock -> [ModifierId]
    unpackInterlinksFromFullBlock = unpackInterlinks . (Api.extension :: Api.FullBlock -> Api.Extension)

-- | Predefined key is used for storing interlinks vector - the first byte of
-- the key is 0x01, the second one corresponds to index of the link in the
-- vector and the value contains actual link (32 bytes) prefixed with the
-- number of times it appears in the vector (1 byte).
unpackInterlinks :: Api.Extension -> [ModifierId]
unpackInterlinks =
        Api.fields
    >>> fmap (first unHexJSON)
    >>> filter ((==(Just 1)) . preview (ix 0) . fst)
    >>> fmap (first (preview (ix 1)))
    >>> sortOn fst
    >>> fmap ( (\b -> replicate (maybe 1 fromIntegral (b^?(ix 0))) (ModifierId . BS.drop 1 $ b))
             . unHexJSON . snd)
    >>> mconcat

deriveJSON A.defaultOptions ''PoPowHeader
