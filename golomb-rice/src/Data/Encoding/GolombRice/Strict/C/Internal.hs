{-# LANGUAGE BangPatterns #-}
module Data.Encoding.GolombRice.Strict.C.Internal where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Generics
import Prelude hiding (length, null)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Encoding.GolombRice.Strict.C.Raw as C
import qualified Data.Vector.Storable as VS

newtype GolombRiceWriter = GolombRiceWriter {
    golombRiceWriter       :: (ForeignPtr C.GolombRiceWriter)
  } deriving (Generic)

data GolombRiceReader = GolombRiceReader {
    golombRiceReaderSize   :: !Int
  , golombRiceReaderBuffer :: !(ForeignPtr CUChar)
  , golombRiceReader       :: !(ForeignPtr C.GolombRiceReader)
  } deriving (Generic)

-- | Allocate new writer stream
empty :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> Int -- ^ Preallocated amount of bytes
  -> m GolombRiceWriter
empty p n = liftIO $ do
  wp <- C.golombrice_writer_new n p
  wfp <- newForeignPtr C.golombrice_writer_delete_ptr wp
  pure $ GolombRiceWriter wfp
{-# INLINABLE empty #-}

-- | Start reading golomb rice encoded bytestring. Copies contents of the
-- bytestring. O(n)
fromByteString :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> ByteString
  -> m GolombRiceReader
fromByteString p bs = liftIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, n) -> do
  buff <- mallocForeignPtrBytes n
  wp <- withForeignPtr buff $ \bp -> do
    copyBytes bp (castPtr ptr) n
    C.golombrice_reader_new bp p
  wfp <- newForeignPtr C.golombrice_reader_delete_ptr wp
  pure $ GolombRiceReader n buff wfp
{-# INLINABLE fromByteString #-}

-- | Copy encoded result from writer stream.
toByteString :: MonadIO m
  => GolombRiceWriter
  -> m ByteString
toByteString GolombRiceWriter{..} = liftIO $ withForeignPtr golombRiceWriter $ \w -> do
  n <- C.golombrice_writer_length w
  buff <- C.golombrice_writer_data w
  BS.packCStringLen (castPtr buff, n)
{-# INLINABLE toByteString #-}

-- | Helper that writes down all words into new stream
encodeVector :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> VS.Vector Word64
  -> m GolombRiceWriter
encodeVector p vs = do
  s <- empty p (VS.length vs * 8) -- assume that encoding will be smaller than original buffer 
  encodeWords s vs
  pure s
{-# INLINABLE encodeVector #-}

class GolombRice a where
  -- | Query if stream contains any data
  null :: MonadIO m => a -> m Bool
  null = fmap (== 0) . length
  {-# INLINE null #-}

  -- | Query amount of bytes in stream
  length :: MonadIO m => a -> m Int

instance GolombRice GolombRiceWriter where
  length GolombRiceWriter{..} = liftIO $ withForeignPtr golombRiceWriter C.golombrice_writer_length
  {-# INLINE length #-}

instance GolombRice GolombRiceReader where
  length GolombRiceReader{..} = liftIO $ withForeignPtr golombRiceReader C.golombrice_reader_length
  {-# INLINE length #-}

-- | Write down word into golomb rice stream.
encodeWord :: MonadIO m
  => GolombRiceWriter
  -> Word64
  -> m ()
encodeWord GolombRiceWriter{..} w = liftIO $ withForeignPtr golombRiceWriter $ \p ->
  C.golombrice_writer_encode_word p w
{-# INLINE encodeWord #-}

-- | Write down many words into golomb rice stream
encodeWords :: MonadIO m
  => GolombRiceWriter
  -> VS.Vector Word64
  -> m ()
encodeWords GolombRiceWriter{..} vs = liftIO $ withForeignPtr golombRiceWriter $ \p -> do
  let (vsf, n) = VS.unsafeToForeignPtr0 vs
  withForeignPtr vsf $ \vsp -> C.golombrice_writer_encode_words p vsp n
{-# INLINABLE encodeWords #-}

-- | Read next word from the golomb rice stream.
decodeWord :: MonadIO m
  => GolombRiceReader
  -> m Word64
decodeWord GolombRiceReader{..} = liftIO $ withForeignPtr golombRiceReader $ \p ->
  C.golombrice_reader_decode_word p
{-# INLINE decodeWord #-}

-- | Read next N words from the golomb rice stream.
decodeWords :: MonadIO m
  => GolombRiceReader
  -> Int
  -> m (VS.Vector Word64)
decodeWords GolombRiceReader{..} n = liftIO $ withForeignPtr golombRiceReader $ \p -> do
  buff <- mallocForeignPtrBytes (n * 8)
  i <- withForeignPtr buff $ \bptr -> C.golombrice_reader_decode_words p bptr n
  pure $ VS.unsafeFromForeignPtr0 buff i
{-# INLINABLE decodeWords #-}

-- | Helper that tells fold either to continue or stop scanning
data Shortcut a = Next !a | Stop !a
  deriving (Show, Eq, Generic)

-- | Fold over stream of words
foldl
  :: MonadIO m
  => (a -> Word64 -> IO (Shortcut a)) -- ^ Folding function with stop condition
  -> a -- ^ Start accumulator
  -> GolombRiceReader
  -> m a -- ^ End accumulator
foldl f a0 !s = liftIO $ withForeignPtr (golombRiceReaderBuffer s) $ const $ go a0 -- Fixes https://github.com/hexresearch/ergvein/issues/558
  where
    go !a = do
      empty <- null s
      if empty then pure a else do
        w <- decodeWord s
        sh <- f a w
        case sh of
          Next a' -> go a'
          Stop a' -> pure a'
{-# INLINABLE foldl #-}
