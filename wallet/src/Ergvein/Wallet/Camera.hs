module Ergvein.Wallet.Camera(
    openCamera
  , getResultCamera
  , waiterResultCamera
  , debugCameraPage
  ) where

import Data.Text (Text)
import Data.Text as T
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native

openCamera :: MonadFrontBase t m => Event t () -> m (Event t ())
openCamera e = runOnUiThread $ ffor e $ \_ -> do
  cameraWork ""
  pure ()

getResultCamera :: MonadFrontBase t m => Event t () -> m (Event t Text)
getResultCamera e = runOnUiThread $ ffor e $ const cameraGetResult

waiterResultCamera :: MonadFrontBase t m => Event t () -> m (Event t Text)
waiterResultCamera startE = mdo
  resE <- getResultCamera $ leftmost [startE, nextE]
  nextE <- delay 1.0 $ fforMaybe resE $ \v -> case T.null v of
                        True  -> Just ()
                        False -> Nothing
  pure $ fforMaybe resE $ \v -> case T.null v of
          True  -> Nothing
          False -> Just v

debugCameraPage :: MonadFrontBase t m => m ()
debugCameraPage = do
  cameraE <- outlineButton ("Debug QR scan"::Text)
  openE <- openCamera cameraE
  openGoE <- delay 1.0 openE
  resE <- waiterResultCamera openGoE
  resD <- holdDyn "RESULT" resE
  h4 $ dynText resD
  pure ()
