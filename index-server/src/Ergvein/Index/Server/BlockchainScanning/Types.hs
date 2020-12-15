module Ergvein.Index.Server.BlockchainScanning.Types where

import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import Control.DeepSeq
import Data.ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Word
import GHC.Generics
import Data.Map.Strict (Map)

import qualified Network.Haskoin.Transaction        as HK

data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: !Currency
  , blockMetaBlockHeight   :: !BlockHeight
  , blockMetaHeaderHash :: !ShortByteString
  , blockMetaPreviousHeaderBlockHash :: !ShortByteString
  , blockMetaAddressFilter :: !ByteString
  }

data TxInfo = TxInfo
  { txInfoHash         :: !TxHash
  , txInfoBytes        :: !ByteString
  , txInfoUnspent      :: !Word32
  } deriving (Show, Generic)

instance NFData TxInfo

data TxInfoStored = TxInfoStored
  { txStoredHash    :: !TxHash
  , txStoredTx      :: !HK.Tx
  , txStoredHeight  :: !Word32
  , txStoredMeta    :: !(Maybe TxRecMeta)
  } deriving (Generic)

instance NFData TxInfoStored

data BlockInfo = BlockInfo
  { blockInfoMeta       :: !BlockMetaInfo
  , spentTxRecs         :: ![TxInfoStored]
  , blockContentTxInfos :: ![TxInfo]
  }

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }
