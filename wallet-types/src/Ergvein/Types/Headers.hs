module Ergvein.Types.Headers(
    filterStartingHeight
  , headerCheckpoints
  ) where

import Data.Word
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HB

-- | The starting height for filters downloader. Start from SegWit adoption
filterStartingHeight :: Coin -> BlockHeight
filterStartingHeight cur = case cur of
  ERGO -> 0
  TERGO -> 0
  BTC -> 481824
  TBTC -> 1722763
  RTBTC -> 1
{-# INLINE filterStartingHeight #-}

-- | Checkpoints for header scanner that we updated periodically to speedup heigh
-- syncing.
headerCheckpoints :: Coin -> (Word32, V.Vector (HB.BlockHeight, HB.BlockHash))
headerCheckpoints cur = case cur of
  -- 1598198546 is the Timestamp of the block at the 1808161 height
  TBTC -> (1598198546,) $ V.fromList $ [
      (1863500, "000000008e0727f007ec2d2494383ba1ee2b54740dd11d0f3611800f85dd9f25")
    , (1863000, "00000000000000bd766af4265d16226b3ca8271587850c2a880fca173e3cd0d8")
    , (1808161, "00000000c4bd492ae5714a6eccc5d83e9e6a9efcee04d78fa573831c30f98a12")
    , (1808160, "00000000346524de79570af5e3c6fcf8ab9e10e82dae9015842809d615281798")
    , (900000,  "0000000000356f8d8924556e765b7a94aaebc6b5c8685dcfa2b1ee8b41acd89b")
    , (500000,  "000000000001a7c0aaa2630fbb2c0e476aafffc60f82177375b2aaa22209f606")
    , (250000,  "0000000005910c146e4e8d71e8aa6617393738a9794b43cf113076dbaf08460b")
    , (0,       "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943")
    ]
  BTC -> (0, ) $ V.fromList $ [
      (653300, "00000000000000000001d4a8624b7d8f4c5288ac33c23ad29d7d8fbf822ade0f")
    , (653000, "000000000000000000076bafbf65e8effa77952273d5673d2df789b4fef4f9b4")
    , (645000, "000000000000000000034e21f4bb3e31c4f58856763e774102cf8a478a2daf00")
    , (500000, "00000000000000000024fb37364cbf81fd49cc2d51c09c75c35433c3a1945d04")
    , (210000, "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e")
    , (11111,  "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d")
    , (0,      "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048")
    ]
  RTBTC -> (0, ) $ V.fromList $ []
  ERGO -> (0, ) $ V.fromList $ [] -- TODO: Ergo here
  TERGO -> (0, ) $ V.fromList $ []
{-# INLINE headerCheckpoints #-}