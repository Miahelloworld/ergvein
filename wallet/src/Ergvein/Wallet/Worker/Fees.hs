module Ergvein.Wallet.Worker.Fees
  (
    feesWorker
  ) where

import Data.Maybe
import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees
import Ergvein.Types.Network
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Util

import qualified Ergvein.Types.Currency as ETC
import qualified Data.Set as S
import qualified Data.Map.Strict as M

feesTimeout :: NominalDiffTime
feesTimeout = 60

feesWorker :: MonadFront t m => m ()
feesWorker = do
  feeRef  <- getFeesRef
  cursD   <- getActiveCursD
  buildE  <- getPostBuild
  te      <- fmap void $ tickLossyFromPostBuildTime feesTimeout
  tickE   <- delay 1 $ leftmost [te, void $ updated cursD, buildE]
  net     <- getNetworkType
  let goE = attachWith (\cs _ -> fmap currencyToCurrencyCode $ fmap (flip coinByNetwork net) . S.toList $ cs) (current cursD) tickE
  respE <- requestRandomIndexer $ ((ETC.Bitcoin, ) . MFeeRequest) <$> goE -- TODO: Fix this for multiple currencies
  let feesE = fforMaybe respE $ \case
        (_, MFeeResponse fees) -> Just $ repack net fees
        _ -> Nothing
  performFork_ $ ffor feesE $ \fm -> modifyExternalRef_ feeRef $ \fm' -> M.union fm fm'
  where
    repack :: NetworkType -> [FeeResp] -> M.Map ETC.Currency FeeBundle
    repack net fees = M.fromList $ catMaybes $ ffor fees $ \case
      FeeRespBTC _ bndl -> Just (ETC.Bitcoin, bndl)
      FeeRespGeneric cur h m l -> do
        c <- currencyCodeToCurrency net cur
        let bndl = FeeBundle (h,h) (m,m) (l,l)
        pure (ETC.coinCurrency c, bndl)
