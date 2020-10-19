{-# LANGUAGE OverloadedLists #-}
module Ergvein.Faucet.Frontend.Elements(
    container
  , row
  , column
  , column10
  , column20
  , column25
  , column33
  , column40
  , column50
  , column60
  , column67
  , column75
  , column80
  , column90
  , column100
  , spanClass
  , elClassDyn
  , divClassDyn
  , spanClassDyn
  , h1
  , h2
  , h3
  , h4
  , h5
  , colonize
  , colonize_
  , buttonClass
  , widgetHoldDyn
  , updatedWithInit
  , fixedTimeUpdate
  , handleDangerMsg
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IORef
import Data.Text (Text)
import Data.Time
import Reflex
import Reflex.Dom

container :: DomBuilder t m => m a -> m a
container = divClass "container"

row :: DomBuilder t m => m a -> m a
row = divClass "row"

column :: DomBuilder t m => m a -> m a
column = divClass "column"

column10, column20, column25, column33, column40, column50, column60, column67, column75, column80, column90, column100 :: DomBuilder t m => m a -> m a
column10 = divClass "column column-10"
column20 = divClass "column column-20"
column25 = divClass "column column-25"
column33 = divClass "column column-33"
column40 = divClass "column column-40"
column50 = divClass "column column-50"
column60 = divClass "column column-60"
column67 = divClass "column column-67"
column75 = divClass "column column-75"
column80 = divClass "column column-80"
column90 = divClass "column column-90"
column100 = divClass "column column-100"

spanClass :: DomBuilder t m => Text -> m a -> m a
spanClass = elClass "span"

elClassDyn :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m a
elClassDyn eln classD = elDynAttr eln $ do
  v <- classD
  pure [("class", v)]

divClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
divClassDyn = elClassDyn "div"

spanClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
spanClassDyn = elClassDyn "span"

h1 :: DomBuilder t m => m a -> m a
h1 = el "h1"

h2 :: DomBuilder t m => m a -> m a
h2 = el "h2"

h3 :: DomBuilder t m => m a -> m a
h3 = el "h3"

h4 :: DomBuilder t m => m a -> m a
h4 = el "h4"

h5 :: DomBuilder t m => m a -> m a
h5 = el "h5"

chunked :: Int -> [a] -> [[a]]
chunked n [] = []
chunked n xs = take n xs : chunked n (drop n xs)

-- | Traverse container and render widgets for each element in rows
colonize :: (DomBuilder t m)
  => Int -- ^ Amount of columns in row
  -> [a] -- ^ Collection of data
  -> (a -> m b) -- ^ Widget
  -> m [b] -- ^ Results
colonize n as w = fmap concat $ traverse (row . traverse (column . w)) $ chunked n as

-- | Traverse container and render widgets for each element in rows
colonize_ :: (DomBuilder t m)
  => Int -- ^ Amount of columns in row
  -> [a] -- ^ Collection of data
  -> (a -> m b) -- ^ Widget
  -> m () -- ^ Results
colonize_ n as w = traverse_ (row . traverse_ (column . w)) $ chunked n as

-- | Button with CSS classes
buttonClass :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m (Event t ())
buttonClass classVal sd = do
  (e, _) <- elAttr' "button" [("class", classVal), ("href", "javascript:void(0)")] $ dynText sd
  return $ domEvent Click e

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . (DomBuilder t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
widgetHoldDyn maD = do
  ma <- sample . current $ maD
  widgetHold ma $ updated maD

-- | Same as 'updated', but fires init value with 'getPostBuild'
updatedWithInit :: PostBuild t m => Dynamic t a -> m (Event t a)
updatedWithInit da = do
  buildE <- getPostBuild
  pure $ leftmost [updated da, current da `tag` buildE]

-- | Updates dynamic only at fixed rate (to prevent GUI from lagging)
fixedTimeUpdate :: forall t m a . (DomBuilder t m, MonadIO m, MonadSample t m, PerformEvent t m, MonadIO (Performable m), PostBuild t m, MonadHold t m, TriggerEvent t m, MonadFix m)
  => NominalDiffTime
  -> Dynamic t a
  -> m (Dynamic t a)
fixedTimeUpdate step da = do
  a0 <- sample . current $ da
  ref <- liftIO $ newIORef (False, a0)
  performEvent_ $ ffor (updated da) $ \a -> liftIO $ writeIORef ref (True, a)
  tickE <- tickLossyFromPostBuildTime step
  valE <- performEventAsync $ ffor tickE $ const $ \fire -> void . liftIO . forkIO $ do
    (dirty, a) <- readIORef ref
    if dirty then do
      writeIORef ref (False, a)
      fire $! Just a
    else fire Nothing
  holdDyn a0 $ fmapMaybe id valE

handleDangerMsg :: MonadWidget t m => Event t (Either Text a) -> m (Event t a)
handleDangerMsg e = do
  widgetHold (pure ()) $ ffor e $ \case
    Right _ -> pure ()
    Left er -> row $ column $ spanClass "danger" $ text er
  pure $ fmapMaybe (either (const Nothing) Just) e
