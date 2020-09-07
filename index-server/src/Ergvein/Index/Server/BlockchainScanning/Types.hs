module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block

import Control.DeepSeq
import Data.ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Word
import GHC.Generics

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: Currency
  , blockMetaBlockHeight   :: BlockHeight
  , blockMetaHeaderHashHexView :: ShortByteString
  , blockMetaPreviousHeaderBlockHashHexView :: ShortByteString
  , blockMetaAddressFilterHexView :: ByteString
  }

data TxInfo = TxInfo
  { txHash         :: TxHash
  , txBytes        :: ByteString
  , txOutputsCount :: Word32
  } deriving (Show, Generic)

instance NFData TxInfo

data BlockInfo = BlockInfo
  { blockInfoMeta       :: BlockMetaInfo
  , spentTxsHash        :: [TxHash]
  , blockContentTxInfos :: [TxInfo]
  }

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }
