{-# LANGUAGE OverloadedLists #-}
module Ergvein.Faucet.Frontend.Inputs(
    labeled
  , labeledText
  , staticField
  , textField
  , textMayField
  , readField
  , readMayField
  ) where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text, strip)
import Data.Text.Read (rational)
import Reflex
import Reflex.Dom
import Ergvein.Faucet.Frontend.Elements

import qualified Data.Text as T

labeled :: forall t m a . MonadWidget t m => Text -> m a -> m a
labeled lbl ma = row $ do
  column25 $ text lbl
  column75 $ ma

labeledText :: forall t m . MonadWidget t m => Text -> Dynamic t Text -> m ()
labeledText lbl vD = labeled lbl $ dynText vD

staticField :: forall t m . MonadWidget t m => Text -> Dynamic t Text -> m ()
staticField lbl vD = do
  v0 <- sample . current $ vD
  void $ labeled lbl $ textInput (def :: TextInputConfig t) {
        _textInputConfig_initialValue = v0
      , _textInputConfig_setValue = updated vD
      , _textInputConfig_attributes = pure [("disabled", "")]
      }

textField :: MonadWidget t m => Text -> Dynamic t Text -> m (Dynamic t Text)
textField lbl vD = do
  v0 <- sample . current $ vD
  i <- labeled lbl $ textInput def {
      _textInputConfig_initialValue = v0
    , _textInputConfig_setValue = updated vD
    }
  pure $ _textInput_value i

textMayField :: MonadWidget t m => Text -> Dynamic t (Maybe Text) -> m (Dynamic t (Maybe Text))
textMayField lbl vD = do
  v0 <- sample . current $ vD
  i <- labeled lbl $ textInput def {
      _textInputConfig_initialValue = fromMaybe "" v0
    , _textInputConfig_setValue = updated $ fromMaybe "" <$> vD
    }
  let stripEmpty v
        | T.null (T.strip v) = Nothing
        | otherwise = Just v
  pure $ fmap stripEmpty $ _textInput_value i

readField :: (MonadWidget t m, Show a, Read a) => Text -> Dynamic t a -> m (Dynamic t a)
readField lbl vD = do
  txtD <- textField lbl (fmap (T.pack . show) vD)
  pure $ fmap (read . T.unpack) txtD

readMayField :: (MonadWidget t m, Show a, Read a) => Text -> Dynamic t (Maybe a) -> m (Dynamic t (Maybe a))
readMayField lbl vD = do
  txtD <- textMayField lbl (fmap (fmap (T.pack . show)) vD)
  pure $ fmap (fmap (read . T.unpack)) txtD
