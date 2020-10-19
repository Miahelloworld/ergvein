{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Faucet.Frontend.Client(
  -- * Whole client for API
    AsReflex
  , faucetClient
  , faucetClient_
  -- * Shortcut types
  , DBody
  , DCapture
  , DParam
  , DEndpoint
  -- * Endpoints
  , getBalance
  , mineBlocks
  , getHeight
  , sendToAddress
  , setBlockInterval
  , getBlockInterval
  ) where

import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Functor (void)
import GHC.TypeLits
import Reflex                            hiding (Response)
import Reflex.Dom                        hiding (askEvents, Response, Client)
import Servant.API.Generic
import Servant.Reflex
import Ergvein.Faucet.API hiding (sendToAddress)
import Ergvein.Faucet.Frontend.Elements
import Ergvein.Faucet.Frontend.Monad
import Ergvein.Faucet.Shared

-- | Shortcut for request body
type DBody t a = Dynamic t (Either Text a)
-- | Shortcut for request capture clause
type DCapture t a = DBody t a
-- | Shortcut for query body
type DParam t a = Dynamic t (QParam a)
-- | Shortcut for endpoint ending
type DEndpoint t m a =  Event t () -> m (Event t (ReqResult () a))

-- | Helper for servant-generic to convert API structs into servant-reflex client
data AsReflex (t :: *) (m :: * -> *) (tag :: *)
instance GenericMode (AsReflex t m tag) where
  type AsReflex t m tag :- api = Client t m api tag

-- | Convert req result to text
textifyResult :: ReqResult () a -> Either Text a
textifyResult r = case r of
  ResponseSuccess _ a _ -> Right a
  ResponseFailure _ e xhr -> case _xhrResponse_responseText xhr of
    Just body -> Left body
    Nothing -> Left e
  RequestFailure _ e -> Left $ showt e

-- | Convert req result to text
textifyResultTagged :: ReqResult tag a -> Either (tag, Text) (tag, a)
textifyResultTagged r = case r of
  ResponseSuccess tg a _ -> Right (tg, a)
  ResponseFailure tg e xhr -> case _xhrResponse_responseText xhr of
    Just body -> Left (tg, body)
    Nothing -> Left (tg, e)
  RequestFailure tg e -> Left (tg, showt e)

-- | Derive client endpoints for given monad
faucetClient :: forall t m tag . MonadWidget t m => Proxy m -> Proxy tag -> XenoFaucetAPI (AsReflex t m tag)
faucetClient pm ptag = fromServant $ client (Proxy :: Proxy (ToServantApi XenoFaucetAPI)) pm ptag (pure (BasePath "/"))

-- | Derived client with no tag
faucetClient_ :: forall t m . MonadWidget t m => Proxy m -> XenoFaucetAPI (AsReflex t m ())
faucetClient_ pm = faucetClient pm (Proxy :: Proxy ())

-- | Get faucet balance
getBalance :: forall t m . MonadWidget t m => Event t Currency -> m (Event t Double)
getBalance e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- getBalanceEndpoint (faucetClient_ (Proxy :: Proxy m)) currD (void e)
  handleDangerMsg $ fmap textifyResult resE

-- | Produce new blocks
mineBlocks :: forall t m . MonadWidget t m => Dynamic t Word -> Event t Currency -> m (Event t ())
mineBlocks countD e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- mineBlocksEndpoint (faucetClient_ (Proxy :: Proxy m)) currD (Right <$> countD) (void e)
  handleDangerMsg $ fmap textifyResult resE

-- | Get blockchain height
getHeight :: forall t m . MonadWidget t m => Event t Currency -> m (Event t Word)
getHeight e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- getHeightEndpoint (faucetClient_ (Proxy :: Proxy m)) currD (void e)
  handleDangerMsg $ fmap textifyResult resE

-- | Send money to address
sendToAddress :: forall t m . MonadWidget t m => Dynamic t (Either Text SendToAddress) -> Event t Currency -> m (Event t Text)
sendToAddress valD e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- sendToAddressEndpoint (faucetClient_ (Proxy :: Proxy m)) currD valD (void e)
  handleDangerMsg $ fmap textifyResult resE

-- | Set block mining interval
setBlockInterval :: forall t m . MonadWidget t m => Dynamic t (Maybe NominalDiffTime) -> Event t Currency-> m (Event t ())
setBlockInterval valD e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- setBlockIntervalEndpoint (faucetClient_ (Proxy :: Proxy m)) currD (Right <$> valD) (void e)
  handleDangerMsg $ fmap textifyResult resE

-- | Get current block mining interval
getBlockInterval :: forall t m . MonadWidget t m => Event t Currency -> m (Event t (Maybe NominalDiffTime))
getBlockInterval e = do
  currD <- holdDyn (Left "undefined currency") $ Right <$> e
  resE <- getBlockIntervalEndpoint (faucetClient_ (Proxy :: Proxy m)) currD (void e)
  handleDangerMsg $ fmap textifyResult resE
