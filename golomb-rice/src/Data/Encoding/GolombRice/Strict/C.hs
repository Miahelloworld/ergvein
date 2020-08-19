module Data.Encoding.GolombRice.Strict.C(
  -- * Writer
    GolombRiceWriter
  , golombRiceWriterP
  , empty
  , singleton
  , fromList
  , fromVector
  , toByteString
  , encodeWord
  , encodeWords
  , toReaderUnsafe
  -- * Reader
  , GolombRiceReader
  , toList
  , toVector
  , fromByteString
  , decodeWord
  , decodeWords
  -- * Shared generic ops
  , GolombRice(..)
  ) where

import Data.Encoding.GolombRice.Strict.C.Internal
