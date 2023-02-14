{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module ENCS.Main where

import           Control.Monad                   (unless, void)
import           Control.Monad.Reader            (asks)
import           Data.Aeson                      (FromJSON (..), eitherDecodeFileStrict)
import           Data.Default                    (def)
import           Data.Functor                    ((<&>))
import           Data.List                       (tails)
import           GHC.Generics                    (Generic)
import           Plutus.V2.Ledger.Api            (Address)
import qualified PlutusTx.Prelude                as Plutus
import           Cardano.Api                     (writeFileJSON)
import           Cardano.Server.Config           (decodeOrErrorFromFile)
import           Cardano.Server.Internal         (AppM, Env (..), HasServer (..), getNetworkId, runAppM)
import           Cardano.Server.Tx               (checkForCleanUtxos, mkTx)
import           Cardano.Server.Utils.ChainIndex (ChainIndex (Kupo))
import           Control.Monad.IO.Class          (MonadIO (..))
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           ENCOINS.ENCS.Distribution       (mkDistribution, processDistribution)
import           ENCOINS.ENCS.Distribution.IO    (verifyDistribution)
import           ENCOINS.ENCS.OffChain           (distributionTx, encsMintTx)
import           ENCOINS.ENCS.OnChain            (ENCSParams, distributionValidatorAddresses)
import           ENCS.Opts                       (ServerMode (..), runWithOpts)
import           PlutusAppsExtra.IO.Wallet       (ownAddresses)
import           PlutusAppsExtra.Utils.Address   (addressToBech32)

runENCSApp :: IO ()
runENCSApp = do
    mode <- runWithOpts
    runAppM @ENCSApp $ case mode of
        Run            -> serverIdle
        Setup          -> serverSetup
        Verify from to -> verify from to

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

    defaultChainIndex = Kupo

verify :: FilePath -> FilePath -> AppM ENCSApp ()
verify from to = do
        encsParams <- asks $ envENCSParams . envAuxiliary
        distribution <- liftIO $ eitherDecodeFileStrict from >>= either fail (pure . processDistribution)
        res <- liftIO $ verifyDistribution encsParams distribution
        either failure (void . liftIO . writeFileJSON to) res
    where
        failure addr = do
            newtworkId <- getNetworkId
            let prettyAddr = fromMaybe (T.pack $ show addr) $ addressToBech32 newtworkId addr
            liftIO $ T.putStrLn $ "An error occurred while verifying the distribution. First failed address: " <> prettyAddr