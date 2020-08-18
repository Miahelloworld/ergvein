module Data.Encoding.GolombRice.Strict.C.Raw where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr

data GolombRiceWriter
data GolombRiceReader

{-
  Writer.
-}

foreign import ccall unsafe "golombrice_writer_new"
  golombrice_writer_new :: IO (Ptr GolombRiceWriter)

foreign import ccall unsafe "golombrice_writer_delete"
  golombrice_writer_delete :: Ptr GolombRiceWriter -> IO ()

foreign import ccall unsafe "&golombrice_writer_delete"
  golombrice_writer_delete_ptr :: FunPtr (Ptr GolombRiceWriter -> IO ())

foreign import ccall unsafe "golombrice_writer_init"
  golombrice_writer_init :: Ptr GolombRiceWriter -> Ptr CUChar -> Int -> IO ()

foreign import ccall unsafe "golombrice_writer_length"
  golombrice_writer_length :: Ptr GolombRiceWriter -> IO Int

foreign import ccall unsafe "golombrice_writer_data"
  golombrice_writer_data :: Ptr GolombRiceWriter -> IO (Ptr CUChar)

foreign import ccall unsafe "golombrice_writer_encode_word"
  golombrice_writer_encode_word :: Ptr GolombRiceWriter -> Word64 -> IO ()

foreign import ccall unsafe "golombrice_writer_encode_words"
  golombrice_writer_encode_words :: Ptr GolombRiceWriter -> Ptr Word64 -> Int -> IO ()

{-
  Reader.
-}

foreign import ccall unsafe "golombrice_reader_new"
  golombrice_reader_new :: IO (Ptr GolombRiceReader)

foreign import ccall unsafe "golombrice_reader_delete"
  golombrice_reader_delete :: Ptr GolombRiceReader -> IO ()

foreign import ccall unsafe "&golombrice_reader_delete"
  golombrice_reader_delete_ptr :: FunPtr (Ptr GolombRiceReader -> IO ())

foreign import ccall unsafe "golombrice_reader_init"
  golombrice_reader_init :: Ptr GolombRiceReader -> Ptr CUChar -> Int -> IO ()

foreign import ccall unsafe "golombrice_reader_length"
  golombrice_reader_length :: Ptr GolombRiceReader -> IO Int

foreign import ccall unsafe "golombrice_reader_decode_word"
  golombrice_reader_decode_word :: Ptr GolombRiceReader -> IO Word64

foreign import ccall unsafe "golombrice_reader_decode_words"
  golombrice_reader_decode_words :: Ptr GolombRiceReader -> Ptr Word64 -> Int -> IO ()
