{-
  Implementation of ERGO connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Node.ERGO
  (
    ERGOType(..)
  , NodeERG
  , initErgoNode
  ) where

import Data.Text

import Control.Monad.IO.Class
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef

import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Wallet.Node.Prim

instance CurrencyRep ERGOType where
  curRep _ = Ergo

-- | TODO: Change this once actual connection is implemented
instance HasNode ERGOType where
  type NodeReq ERGOType = Text
  type NodeResp ERGOType = Text
  type NodeSpecific ERGOType = ()

initErgoNode :: (Reflex t, TriggerEvent t m, MonadIO m) => NetworkType -> SockAddr -> Event t NodeMessage -> m (NodeERG t)
initErgoNode netType url _ = do
  statRef <- newExternalRef Nothing
  pure $ NodeConnection {
      nodeconCurrency   = Ergo
    , nodeconNetwork    = netType
    , nodeconUrl        = url
    , nodeconStatus     = statRef
    , nodeconOpensE     = never
    , nodeconCloseE     = never
    , nodeconRespE      = never
    , nodeconExtra      = ()
    , nodeconIsUp       = pure False
    , nodecondoLog      = False
    }
