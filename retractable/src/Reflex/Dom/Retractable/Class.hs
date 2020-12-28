-- |
-- Module      : Reflex.Dom.Retractable.Class
-- Copyright   : (c) 2019 ATUM SOLUTIONS AG
-- License     : MIT
-- Maintainer  : ncrashed@protonmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Tagless final interface to the retractable stack of widgets. To use the API
-- drop `retractStack` into your code where you want to display widgets with
-- history. For instance, turn all your frontend into retractable widget:
--
-- @
-- import Control.Monad
-- import Reflex.Dom
-- import Reflex.Dom.Retractable.Class
--
-- frontend :: (MonadWidget t m, MonadRetract t m) => m ()
-- frontend = void $ retractStack $ pageA 42
--
-- pageA :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
-- pageA n = do
--    e <- button "Go page B"
--    void $ nextWidget $ ffor e $ const Retractable {
--        retractableNext = pageB $ n + 1
--      , retractablePrev = Just $ pure $ pageA n
--      }
--
-- pageB :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
-- pageB n = do
--   e <- button "Go page A"
--   void $ nextWidget $ ffor e $ const  Retractable {
--        retractableNext = pageA $ n + 1
--      , retractablePrev = Just $ pure $ pageB n
--      }
-- @
--
module Reflex.Dom.Retractable.Class(
    Retractable(..)
  , morphRetractable
  , MonadRetract(..)
  , retractStack
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import GHC.Generics (Generic)
import Reflex
import Reflex.Network

-- | Information about widget that attaches information how to return to the
-- widget.
data Retractable t m = Retractable {
  -- | Which widget we are switching in
  retractableNext :: m ()
  -- | Possible return to the current widget. 'Nothing' means that the
  -- current widget is not rembered in retract stack and is forgoten.
  -- Dynamic allows to save internal state of widget on return.
, retractablePrev :: Maybe (Dynamic t (m ()))
} deriving (Generic)

-- | Helper to transform underlying monad in `Retractable`
morphRetractable :: Reflex t => (forall a . n a -> m a) -> Retractable t n -> Retractable t m
morphRetractable f (Retractable next prev) = Retractable (f next) (fmap f <$> prev)

-- | Defines context of widget that can switch control to next widget
-- and can be returned back with preserving of state.
class (MonadHold t m, MonadFix m, Reflex t, Adjustable t m) => MonadRetract t m where
  -- | Switch current widget to the next widget. Returns event that fires
  -- when the switching is about to happen.
  nextWidget :: Event t (Retractable t m) -> m (Event t ())

  -- | Switch to previous widget in stack. Returns event that fires
  -- when the switchin is about to happen.
  retract :: Event t () -> m (Event t ())

  -- | Wipe retract history to the given amount of items (or all if `Nothing` is passed).
  -- Returns event that fires when the history was changed.
  wipeRetract :: Event t (Maybe Int) -> m (Event t ())

  -- | Get event that fires when any of input events in `nextWidget` is triggered.
  -- It's used for implementation of retractable stack.
  nextWidgetEvent :: m (Event t (Retractable t m))

  -- | Get event that fires when any of input events in `retract` is triggered.
  -- It's used for implementation of retractable stack.
  retractEvent :: m (Event t ())

  -- | Get event that fires when any of input events in `wipeRetract` is triggered.
  -- It's used for implementation of retractable stack.
  wipeRetractEvent :: m (Event t (Maybe Int))

  -- | Get current stack of widget history
  getRetractStack :: m (Dynamic t [Retractable t m])

  -- | Execute subcomputation with given widget history. Affects results of `getRetractStack`.
  withRetractStack :: Dynamic t [Retractable t m] -> m a -> m a

-- | Helper ADT to merge actions with retractable stack
data StackAction t m = StackPush (Retractable t m) | StackPop | StackWipe (Maybe Int)

-- | All body of the widget will be rerendered when some subcomputation emits switching event.
-- Plug the function somewhere close to the root of your reflex frontend and use functions from
-- `MonadRetract` class to control switching content of the widget.
retractStack :: forall t m . MonadRetract t m => m () -> m (Event t ())
retractStack ma = do
  nextE <- fmap StackPush <$> nextWidgetEvent
  backE <- fmap (const StackPop) <$> retractEvent
  wipeE <- fmap StackWipe <$> wipeRetractEvent
  let actionE = leftmost [nextE, backE, wipeE]
  let go :: StackAction t m -> [Retractable t m] -> [Retractable t m]
      go a rs = case a of
        StackPush r -> maybe rs (const $ r : rs) $ retractablePrev r
        StackPop -> drop 1 rs
        StackWipe Nothing -> []
        StackWipe (Just i) -> drop i rs
  stackD :: Dynamic t [Retractable t m] <- foldDyn go [] actionE
  resD <- withRetractStack stackD $ networkHold ma $ flip push actionE $ \case
    StackPush r -> pure . Just $ retractableNext r
    StackPop -> do
      rs <- sample . current $ stackD
      case rs of
        (r : _) | Just maD <- retractablePrev r -> fmap Just . sample . current $ maD
        _ -> pure . Just $ ma
    StackWipe _ -> pure Nothing
  pure $ updated resD

instance MonadRetract t m => MonadRetract t (ReaderT r m) where
  nextWidget e = do
    r <- ask
    lift $ nextWidget (morphRetractable (flip runReaderT r) <$> e)
  {-# INLINE nextWidget #-}
  retract = lift . retract
  {-# INLINE retract #-}
  wipeRetract = lift . wipeRetract
  {-# INLINE wipeRetract #-}
  nextWidgetEvent = do
    e <- lift nextWidgetEvent
    pure $ morphRetractable lift <$> e
  {-# INLINE nextWidgetEvent #-}
  retractEvent = lift retractEvent
  {-# INLINE retractEvent #-}
  wipeRetractEvent = lift wipeRetractEvent
  {-# INLINE wipeRetractEvent #-}
  getRetractStack = do
    st <- lift getRetractStack
    pure $ fmap (morphRetractable lift) <$> st
  {-# INLINE getRetractStack #-}
  withRetractStack st ma = do
    r <- ask
    let st' = fmap (morphRetractable (flip runReaderT r)) <$> st
    lift $ withRetractStack st' (runReaderT ma r)
  {-# INLINE withRetractStack #-}
