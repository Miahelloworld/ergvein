{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Data.Functor (void)
import Network.Haskoin.Transaction
import Test.QuickCheck
import Test.QuickCheck.Instances()

import Test.Generators()
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Types.Currency

--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

_prop_encdec_lastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
_prop_encdec_lastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where
    enc = egvSerialize Bitcoin msg
    dec = egvDeserialize Bitcoin enc

_prop_encdec_ScannedHeightRec :: ScannedHeightRec -> Bool
_prop_encdec_ScannedHeightRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_TxRecBytes :: TxRecBytes -> Bool
_prop_encdec_TxRecBytes msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_TxRecMeta :: TxRecMeta -> Bool
_prop_encdec_TxRecMeta msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_BlockMetaRec :: BlockMetaRec -> Bool
_prop_encdec_BlockMetaRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_KnownPeerRecItem :: KnownPeerRecItem -> Bool
_prop_encdec_KnownPeerRecItem msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_KnownPeersRec :: KnownPeersRec -> Bool
_prop_encdec_KnownPeersRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
_prop_encdec_LastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
_prop_encdec_LastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg

_prop_encdec_RollbackSequence :: RollbackSequence -> Bool
_prop_encdec_RollbackSequence msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg

prop_encdec_TxIn :: TxIn -> Bool
prop_encdec_TxIn msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg

prop_encdec_TxOut :: TxOut -> Bool
prop_encdec_TxOut msg = either (const False) (msg ==) dec
  where dec = egvDeserialize Bitcoin $ egvSerialize Bitcoin msg
--------------------------------------------------------------------------
-- main

pure [] -- Split splices so quickCheckAll can find declarations above

main :: IO ()
main = void $quickCheckAll

--------------------------------------------------------------------------
-- the end.
