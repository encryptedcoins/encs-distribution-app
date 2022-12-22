{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module ENCS.Main where

import Control.Monad             (void)
import Control.Monad.Reader      (asks)
import Control.Monad.Catch       (Exception)
import Data.Aeson                (FromJSON(..))
import Data.Functor              ((<&>))
import Data.List                 (tails)
import ENCS.Opts
import ENCOINS.ENCS.Distribution (mkDistribution, processDistribution)
import ENCOINS.ENCS.OffChain     (encsCurrencySymbol, encsMintTx, distributionTx)
import ENCOINS.ENCS.OnChain      (ENCSParams, DistributionValidatorParams, distributionFee, distributionFeeCount)
import GHC.Generics              (Generic)
import Plutus.V2.Ledger.Api      (Address)
import Servant                   (NoContent)
import Server.Config             (decodeOrErrorFromFile)
import Server.Endpoints.SubmitTx (HasSubmitTxEndpoint(..))
import Server.Internal           (HasServer(..), Env(..), SetupM, runSetupM, loadEnv)
import Server.Tx                 (mkTx)

runENCSApp :: IO ()
runENCSApp = do
    mode <- runWithOpts
    env <- loadEnv
    runSetupM @ENCSApp env $ case mode of
        Run   -> cycleTx
        Setup -> setupServer

data ENCSApp

data ENCSEnv = ENCSEnv
    { envENCSParams :: ENCSParams
    , envAddrList   :: [(Address, Integer)]
    } deriving (Show, Generic, FromJSON)

instance HasServer ENCSApp where

    type AuxiliaryEnvOf ENCSApp = ENCSEnv

    loadAuxiliaryEnv _ = do
        envENCSParams <- decodeOrErrorFromFile "testnet/encs-params.json"
        envAddrList   <- decodeOrErrorFromFile "testnet/addresses.json" <&> processDistribution
        pure ENCSEnv{..}

    type RedeemerOf ENCSApp = ()

    getCurrencySymbol = encsCurrencySymbol <$> asks (envENCSParams . envAuxiliary)

    processTokens _ = pure ()

    setupServer = do
        ENCSEnv{..} <- asks envAuxiliary
        let amtF = distributionFee * distributionFeeCount
            distribution = mkDistribution envENCSParams envAddrList amtF
        void $ mkTx [] [encsMintTx envENCSParams distribution]

    cycleTx = do
        ENCSEnv{..} <- asks envAuxiliary
        let amtF = distributionFee * distributionFeeCount
            distribution = mkDistribution envENCSParams envAddrList amtF
        void $ traverse (\d -> mkTx [] [distributionTx d]) $ tails distribution