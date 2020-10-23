module Ergvein.Wallet.Page.Settings.MnemonicExport (
    mnemonicExportPage
  ) where

import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Ergvein.Crypto
import Ergvein.Types.Storage
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Password
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Share
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Serialize as S
import qualified Data.Text as T

mnemonicExportPage :: MonadFront t m => Mnemonic -> m ()
mnemonicExportPage mnemonic = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSMnemonicTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSMnemonicDescr
  passE <- setupPassword =<< submitSetBtn
  _ <- nextWidget $
    ffor passE $ \pass ->
      Retractable
        { retractableNext = mnemonicExportResultPage mnemonic pass,
          retractablePrev = Nothing
        }
  pure ()

mnemonicExportResultPage :: MonadFront t m => Mnemonic -> Password -> m ()
mnemonicExportResultPage mnemonic pass = do
  title <- localized STPSTitle
  let thisWidget = Just $ pure $ mnemonicExportResultPage mnemonic pass
  wrapper True title thisWidget $ divClass "mnemonic-export-page" $ do
    encryptedMnemonic <- liftIO $ encryptMnemonic mnemonic pass
    h4 $ localizedText STPSMnemonicExportMsg
    base64D <- divClass "receive-qr" $ qrCodeWidgetWithData encryptedMnemonic
    let mnemonicClass = if T.null pass then "" else "word-break-all"
    parClass mnemonicClass $ text encryptedMnemonic
    divClass "mnemonic-export-buttons-wrapper" $ do
      copyE <- copyBtn
      _ <- clipboardCopy (encryptedMnemonic <$ copyE)
      when isAndroid $ do
        shareE <- fmap (encryptedMnemonic <$) shareBtn
        _ <- shareShareUrl shareE
        shareQRE <- shareQRBtn
        _ <- shareShareQR $ attachPromptlyDynWithMaybe (\m _ -> (, "qr_code") <$> m) base64D shareQRE
        pure ()

copyBtn :: MonadFront t m => m (Event t ())
copyBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSCopy "fas fa-copy fa-lg"

shareBtn :: MonadFront t m => m (Event t ())
shareBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSShare "fas fa-share-alt fa-lg"

shareQRBtn :: MonadFront t m => m (Event t ())
shareQRBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSShareQR "fas fa-qrcode fa-lg"

encryptMnemonic :: (MonadIO m, MonadRandom m) => Mnemonic -> Password -> m Text
encryptMnemonic mnemonic "" = pure mnemonic
encryptMnemonic mnemonic pass = do
  let mnemonicBS = encodeUtf8 mnemonic
  encryptedMnemonic <- encryptBSWithAEAD mnemonicBS pass
  case encryptedMnemonic of
    Left err -> fail $ T.unpack $ localizedShow English err
    Right encMnemonic -> do
      let encMnemonicBs = S.encode encMnemonic
          mnemonicText = encodeBase58CheckBtc encMnemonicBs
      pure mnemonicText
