{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Monad             (void)
import Control.Monad.Reader      (asks)
import Control.Monad.Catch       (Exception)
import Data.Aeson                (FromJSON(..))
import Data.Functor              ((<&>))
import ENCOINS.ENCS.Distribution (mkDistribution, processDistribution)
import ENCOINS.ENCS.OffChain     (encsCurrencySymbol, encsMintTx, distributionTx)
import ENCOINS.ENCS.OnChain      (ENCSParams, DistributionValidatorParams, distributionFee, distributionFeeCount)
import GHC.Generics              (Generic)
import Plutus.V2.Ledger.Api      (Address)
import Servant                   (NoContent)
import Server.Config             (decodeOrErrorFromFile)
import Server.Endpoints.SubmitTx (HasSubmitTxEndpoint(..))
import Server.Internal           (HasServer(..), Env(..), SetupM)
import Server.Tx                 (mkTx)

data ENCSServer

data ENCSEnv = ENCSEnv
    { envENCSParams :: ENCSParams
    , envAddrList   :: [(Address, Integer)]
    } deriving (Show, Generic, FromJSON)

instance HasServer ENCSServer where

    type AuxiliaryEnvOf ENCSServer = ENCSEnv

    loadAuxiliaryEnv _ = do
        envENCSParams <- decodeOrErrorFromFile "testnet/encsParams.json"
        envAddrList   <- decodeOrErrorFromFile "testnet/addresses.json" <&> processDistribution
        pure ENCSEnv{..}

    type RedeemerOf ENCSServer = ()

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
        doCycleTx distribution

doCycleTx :: DistributionValidatorParams -> SetupM ENCSServer ()
doCycleTx []             = pure ()
doCycleTx distr@(_:rest) = mkTx [] [distributionTx distr] >> doCycleTx rest

instance HasSubmitTxEndpoint ENCSServer where

    type SubmitTxApiResultOf ENCSServer = '[NoContent]

    data SubmitTxErrorOf ENCSServer
        deriving (Show, Exception)

    checkForSubmitTxErros _ = pure ()

    submitTxErrorHanlder = \case