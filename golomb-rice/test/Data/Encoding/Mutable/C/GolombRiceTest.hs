{-# LANGUAGE CPP #-}
module Data.Encoding.Mutable.C.GolombRiceTest where

import           Data.Word
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

#ifdef CBITSTREAM

import           Data.Foldable

import qualified Data.Bitstream.C              as BS
import           Data.Encoding.GolombRice.Strict.C as G

p :: Int
p = 19

spec_basicEncoding :: Spec
spec_basicEncoding = describe "basic test vectors" $ do
  it "empty stream is empty" $ do
    res <- G.null =<< (G.empty p 8 :: IO G.GolombRiceWriter)
    res `shouldBe` True
  it "non empty stream is not empty" $ do
    res <- G.null =<< (G.singleton p 0 :: IO G.GolombRiceWriter)
    res `shouldBe` False
  it "encodes long number" $ do
    let a = 524388
    res <- G.decodeWord =<< toReaderUnsafe =<< G.singleton p a
    res `shouldBe` a

spec_encodingCheck :: Spec
spec_encodingCheck = describe "encoded bits are correct" $ do
  let showBits = fmap $ \v -> if v then '1' else '0'
  let check i is = it ("encodes " <> show i <> " as " <> showBits is) $ do
        g <- G.empty 2 8
        G.encodeWord g i
        s <- BS.unpack =<< BS.fromByteString =<< G.toByteString g
        s `shouldBe` is
  traverse_
    (uncurry check)
    [ (0 , [False, False, False])
    , (1 , [False, False, True])
    , (2 , [False, True, False])
    , (3 , [False, True, True])
    , (4 , [True, False, False, False])
    , (5 , [True, False, False, True])
    , (6 , [True, False, True, False])
    , (7 , [True, False, True, True])
    , (8 , [True, True, False, False, False])
    , (9 , [True, True, False, False, True])
    , (10, [True, True, False, True, False])
    ]

prop_encodingDecodingWord :: Small Word64 -> Property
prop_encodingDecodingWord (Small w) = idempotentIOProperty $ do
  gw <- G.empty p 8
  G.encodeWord gw w
  gr <- G.toReaderUnsafe gw
  rw <- G.decodeWord gr
  e <- G.null gr
  pure $ rw == w && e

prop_singletonHeadWord :: Small Word64 -> Property
prop_singletonHeadWord (Small w) = idempotentIOProperty $ do
  res <- G.decodeWord =<< G.toReaderUnsafe =<< G.singleton p w
  pure $ res == w

prop_encodingDecodingWords :: [Small Word64] -> Property
prop_encodingDecodingWords wss = idempotentIOProperty $ do
  let ws = fmap (\(Small a) -> a) wss
  res <- G.toList =<< G.toReaderUnsafe =<< G.fromList p ws
  pure $ res == ws

prop_encodingDecodingWordsBs :: [Small Word64] -> Property
prop_encodingDecodingWordsBs wss = idempotentIOProperty $ do
  let ws = fmap (\(Small a) -> a) wss
  gs1 <- G.fromList p ws
  bs1 <- G.toByteString gs1
  gs2 <- G.fromByteString (golombRiceWriterP gs1) bs1
  ws2 <- G.toList gs2
  pure $ ws == ws2
#else

spec_basicEncoding :: Spec
spec_basicEncoding = pure ()

spec_encodingCheck :: Spec
spec_encodingCheck = pure ()

prop_encodingDecodingWord :: Small Word64 -> Bool
prop_encodingDecodingWord = const True

prop_singletonHeadWord :: Small Word64 -> Bool
prop_singletonHeadWord = const True

prop_encodingDecodingWords :: [Small Word64] -> Bool
prop_encodingDecodingWords = const True

prop_encodingDecodingWordsBs :: [Small Word64] -> Property
prop_encodingDecodingWordsBs = const True

#endif
