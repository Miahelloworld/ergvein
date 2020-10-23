module Ergvein.Wallet.Storage.AuthInfo (
    initAuthInfo
  , loadAuthInfo
  ) where

import Control.Monad.Except
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Network
import Ergvein.Types.Restore
import Ergvein.Types.Storage
import Ergvein.Wallet.Localization.AuthInfo
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Util

initAuthInfo :: MonadIO m => NetworkType ->  WalletSource -> DerivPrefix -> Mnemonic -> [Currency] -> WalletName -> Password -> m (Either AuthInfoAlert AuthInfo)
initAuthInfo net wt path mnemonic curs login pass = do
  mstorage <- createStorage net (wt == WalletRestored) path mnemonic (login, pass) curs
  case mstorage of
    Left err -> pure $ Left $ CreateStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right AuthInfo {
          _authInfo'storage = s
        , _authInfo'eciesPubKey = toPublic k
        , _authInfo'login = login
        , _authInfo'isUpdate = False
        , _authInfo'isPlain = pass == ""
        }

loadAuthInfo :: (MonadIO m, HasStoreDir m, PlatformNatives) => WalletName -> Password -> m (Either AuthInfoAlert (AuthInfo, Password))
loadAuthInfo login pass = do
  mstorage <- loadStorageFromFile login pass
  case mstorage of
    Left err -> pure $ Left $ LoadStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right (
          AuthInfo {
            _authInfo'storage = s
          , _authInfo'eciesPubKey = toPublic k
          , _authInfo'login = login
          , _authInfo'isUpdate = False
          , _authInfo'isPlain = pass == ""
          }
        , pass
        )
