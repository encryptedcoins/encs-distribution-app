{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module ENCS.Main where

import Cardano.Server.Config             (decodeOrErrorFromFile)
import Cardano.Server.Input              (InputContext(..))
import Cardano.Server.Internal           (HasServer(..), Env(..), runAppM)
import Cardano.Server.Tx                 (mkTx)
import Control.Monad                     (void)
import Control.Monad.Reader              (asks)
import Data.Aeson                        (FromJSON(..))
import Data.Foldable                     (traverse_)
import Data.Functor                      ((<&>))
import Data.List                         (tails)
import ENCS.Opts
import ENCOINS.ENCS.Distribution         (mkDistribution, processDistribution)
import ENCOINS.ENCS.OffChain             (encsMintTx, distributionTx)
import ENCOINS.ENCS.OnChain              (ENCSParams, distributionFee, distributionFeeCount)
import GHC.Generics                      (Generic)
import IO.Wallet                         (ownAddresses)
import Plutus.V2.Ledger.Api              (Address)

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
        envENCSParams <- decodeOrErrorFromFile "testnet/encs-params.json"
        envAddrList   <- decodeOrErrorFromFile "testnet/addresses.json" <&> processDistribution
        pure ENCSEnv{..}

    type InputOf ENCSApp = ()

    serverSetup = do
        ENCSEnv{..} <- asks envAuxiliary
        let amtF = distributionFee * distributionFeeCount
            distribution = mkDistribution envENCSParams envAddrList amtF
        addrs <- ownAddresses
        void $ mkTx addrs emptyContext [encsMintTx envENCSParams distribution]

    serverIdle = do
        ENCSEnv{..} <- asks envAuxiliary
        let amtF = distributionFee * distributionFeeCount
            distribution = mkDistribution envENCSParams envAddrList amtF
        addrs <- ownAddresses
        traverse_ (\d -> mkTx addrs emptyContext [distributionTx d]) $ tails distribution

emptyContext :: InputContext
emptyContext = InputContextServer mempty