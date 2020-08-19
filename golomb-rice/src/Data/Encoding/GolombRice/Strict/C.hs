module Data.Encoding.GolombRice.Strict.C(
  -- * Writer
    GolombRiceWriter
  , empty
  , toByteString
  , encodeWord
  , encodeWords
  -- * Reader
  , GolombRiceReader
  , fromByteString
  , decodeWord
  , decodeWords
  -- * Shared generic ops
  , GolombRice(..)
  ) where

import Data.Encoding.GolombRice.Strict.C.Internal
