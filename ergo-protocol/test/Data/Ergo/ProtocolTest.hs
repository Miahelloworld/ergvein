module Data.Ergo.ProtocolTest where

import Control.Monad
import Data.ByteString.Builder
import Data.Ergo.Protocol
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Utf8
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

import Debug.Trace

traceShowIdHex :: BS.ByteString -> BS.ByteString
traceShowIdHex a = traceShow (B16.encode a) a

prop_encodeDecodeTestnet :: TestnetMessage -> Property
prop_encodeDecodeTestnet msg = property $ traceShowId (decode (traceShowIdHex $ encode msg)) == Right (traceShowId msg)

-- prop_encodeDecodeMainnet :: MainnetMessage -> Property
-- prop_encodeDecodeMainnet msg = property $ decode (encode msg) == Right msg

instance Arbitrary TestnetMessage where
  arbitrary = TestnetMessage <$> arbitrary
  shrink = genericShrink

instance Arbitrary MainnetMessage where
  arbitrary = MainnetMessage <$> arbitrary
  shrink = genericShrink

instance Arbitrary Message where
  arbitrary = MsgHandshake <$> arbitrary
  shrink = genericShrink

instance Arbitrary ProtoVer where
  arbitrary = ProtoVer <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary IP where
  arbitrary = frequency [(3, IPV4 <$> arbitrary), (1, IPV6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)]
  shrink = genericShrink

instance Arbitrary NetAddr where
  arbitrary = NetAddr <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary StateType where
  arbitrary = frequency [(1, pure StateUtxo), (1, pure StateDigest)]

instance Arbitrary OperationModeFeature where
  arbitrary = OperationModeFeature
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary PeerFeature where
  arbitrary = FeatureOperationMode <$> arbitrary
  shrink = genericShrink

instance Arbitrary Handshake where
  arbitrary = Handshake
    <$> arbitrary
    <*> arbitraryTextLimit 255
    <*> arbitrary
    <*> arbitraryTextLimit 255
    <*> arbitrary
    <*> arbitraryVecLimit 10
  shrink v
    | V.length (peerFeatures v) > 0 =
      [ v { peerFeatures = mempty } ]
      ++
      [ v { peerFeatures = V.take i $ peerFeatures v } | i <- [1 .. V.length (peerFeatures v)]  ]
      ++
      [ v { agentName = ""}, v {peerName = ""} ]
      ++
      genericShrink v
    | otherwise = genericShrink v

instance Arbitrary Text where
  arbitrary = genValidUtf8
  shrink v = [ T.drop i v | i <- [1 .. T.length v] ]

arbitraryTextLimit :: Int -> Gen Text
arbitraryTextLimit n = do
  i <- choose (0, n)
  t <- T.pack <$> replicateM i arbitrary
  pure $ fitSize t
  where
    fitSize t | BS.length (encodeUtf8 t) <= n = t
              | otherwise = fitSize $ T.init t

arbitraryVecLimit :: Arbitrary a => Int -> Gen (Vector a)
arbitraryVecLimit n = do
  i <- choose (0, n)
  V.replicateM i arbitrary
