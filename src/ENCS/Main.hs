{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module ENCS.Main where

import           Control.Monad                  (void, unless)
import           Control.Monad.Reader           (asks)
import           Data.Aeson                     (FromJSON(..))
import           Data.Default                   (def)
import           Data.Functor                   ((<&>))
import           Data.List                      (tails)
import           GHC.Generics                   (Generic)
import           Plutus.V2.Ledger.Api           (Address)
import qualified PlutusTx.Prelude               as Plutus

import           Cardano.Server.Config          (decodeOrErrorFromFile)
import           Cardano.Server.Internal        (HasServer(..), Env(..), runAppM)
import           Cardano.Server.Tx              (mkTx, checkForCleanUtxos)
import           ENCS.Opts
import           ENCOINS.ENCS.Distribution      (mkDistribution, processDistribution)
import           ENCOINS.ENCS.OffChain          (encsMintTx, distributionTx)
import           ENCOINS.ENCS.OnChain           (ENCSParams, distributionValidatorAddresses)
import           PlutusAppsExtra.IO.Wallet      (ownAddresses)

runENCSApp :: IO ()
runENCSApp = do
    mode <- runWithOpts
    runAppM @ENCSApp $ case mode of
        Run   -> serverIdle
        Setup -> serverSetup

data ENCSApp

data ENCSEnv = ENCSEnv
    { envENCSParams :: ENCSParams
    , envAddrList   :: [(Address, Integer)]
    } deriving (Show, Generic, FromJSON)

instance HasServer ENCSApp where

    type AuxiliaryEnvOf ENCSApp = ENCSEnv

    loadAuxiliaryEnv _ = do
        envENCSParams <- decodeOrErrorFromFile "encs-params.json"
        envAddrList   <- decodeOrErrorFromFile "distribution.json" <&> processDistribution
        pure ENCSEnv{..}

    type InputOf ENCSApp = ()

    serverSetup = do
        ENCSEnv{..} <- asks envAuxiliary
        let fee          = 100
            nFeeCovered  = Plutus.length envAddrList
            distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
        unless (null distribution) $ do
            let utxos = Just $ head distribution
            addrs <- ownAddresses
            void $ mkTx addrs def [encsMintTx envENCSParams utxos]

    serverIdle = do
        checkForCleanUtxos
        ENCSEnv{..} <- asks envAuxiliary
        let fee          = 100
            nFeeCovered  = Plutus.length envAddrList
            distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
            addrs = distributionValidatorAddresses distribution
        void $ mkTx addrs def $ map distributionTx $ tails distribution
        serverIdle