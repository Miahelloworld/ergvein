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

data GolombRiceWriter = GolombRiceWriter {
    golombRiceWriterSize   :: !Int
  , golombRiceWriterBuffer :: !(ForeignPtr CUChar)
  , golombRiceWriter       :: !(ForeignPtr C.GolombRiceWriter)
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
  buff <- mallocForeignPtrBytes n
  wp <- C.golombrice_writer_new
  wfp <- newForeignPtr C.golombrice_writer_delete_ptr wp
  withForeignPtr buff $ \bp -> C.golombrice_writer_init wp bp p
  pure $ GolombRiceWriter n buff wfp

-- | Reallocate internal buffer to the given size. If the size is smaller than
-- amount of bytes clamps to that
resize :: MonadIO m => Int -> GolombRiceWriter -> m GolombRiceWriter
resize n gs = liftIO $ withForeignPtr (golombRiceWriter gs) $ \wp -> do
  l <- fmap fromIntegral $ C.golombrice_writer_length wp
  let n' = if n < l then l else n
  newBuff <- mallocForeignPtrBytes n'
  withForeignPtr newBuff $ \newp -> withForeignPtr (golombRiceWriterBuffer gs) $ \oldp -> do
    copyBytes newp oldp l
    withForeignPtr (golombRiceWriter gs) $ \wp -> C.golombrice_writer_update_buffer wp newp
  pure $ GolombRiceWriter n' newBuff (golombRiceWriter gs)
{-# INLINABLE resize #-}

-- | Reallocate if needed to handle given amount of additiona bytes.
realloc :: MonadIO m => Int -> GolombRiceWriter -> m GolombRiceWriter
realloc n gs = liftIO $ withForeignPtr (golombRiceWriter gs) $ \wp -> do
  l <- fmap fromIntegral $ C.golombrice_writer_length wp
  if (golombRiceWriterSize gs <= l + n)
    then resize (if l + n < 2*l then 2*l else l + n) gs
    else pure gs
{-# INLINEABLE realloc #-}


-- | Start reading golomb rice encoded bytestring. Copies contents of the
-- bytestring. O(n)
fromByteString :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> ByteString
  -> m GolombRiceReader
fromByteString p bs = liftIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, n) -> do
  buff <- mallocForeignPtrBytes n
  wp <- C.golombrice_reader_new
  wfp <- newForeignPtr C.golombrice_reader_delete_ptr wp
  withForeignPtr buff $ \bp -> do
    copyBytes bp (castPtr ptr) n
    C.golombrice_reader_init wp bp p
  pure $ GolombRiceReader n buff wfp

-- | Copy encoded result from writer stream.
toByteString :: MonadIO m
  => GolombRiceWriter
  -> m ByteString
toByteString GolombRiceWriter{..} = liftIO $ withForeignPtr golombRiceWriterBuffer $ \buff -> do
  n <- withForeignPtr golombRiceWriter C.golombrice_writer_length
  BS.packCStringLen (castPtr buff, n)

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

-- | Write down many words into golomb rice stream
encodeWords :: MonadIO m
  => GolombRiceWriter
  -> VS.Vector Word64
  -> m ()
encodeWords GolombRiceWriter{..} vs = liftIO $ withForeignPtr golombRiceWriter $ \p -> do
  let (vsf, n) = VS.unsafeToForeignPtr0 vs
  withForeignPtr vsf $ \vsp -> C.golombrice_writer_encode_words p vsp n

-- | Read next word from the golomb rice stream.
decodeWord :: MonadIO m
  => GolombRiceReader
  -> m Word64
decodeWord GolombRiceReader{..} = liftIO $ withForeignPtr golombRiceReader $ \p ->
  C.golombrice_reader_decode_word p

-- | Read next N words from the golomb rice stream.
decodeWords :: MonadIO m
  => GolombRiceReader
  -> Int
  -> m (VS.Vector Word64)
decodeWords GolombRiceReader{..} n = liftIO $ withForeignPtr golombRiceReader $ \p -> do
  buff <- mallocForeignPtrBytes (n * 8)
  withForeignPtr buff $ \bptr -> C.golombrice_reader_decode_words p bptr n
  pure $ VS.unsafeFromForeignPtr0 buff n

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
