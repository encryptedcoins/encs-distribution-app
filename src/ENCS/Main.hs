{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase #-}

module ENCS.Main where

import           Cardano.Api                   (writeFileJSON)
import           Cardano.Server.Config         (decodeOrErrorFromFile)
import           Cardano.Server.Internal       (AppM (..), Env (..), HasServer (..), getNetworkId, runAppM)
import           Cardano.Server.Tx             (checkForCleanUtxos, mkTx)
import           Control.Monad                 (unless, void, when)
import           Control.Monad.Catch           (Exception (..), MonadThrow (throwM), handle, catches, Handler (..))
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Control.Monad.Reader          (asks)
import           Data.Aeson                    (FromJSON (..), eitherDecodeFileStrict)
import           Data.Default                  (def)
import           Data.Functor                  ((<&>))
import           Data.List                     (tails)
import           Data.Maybe                    (fromMaybe, isNothing)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           ENCOINS.ENCS.Distribution     (mkDistribution, processDistribution)
import           ENCOINS.ENCS.Distribution.IO  (verifyDistribution)
import           ENCOINS.ENCS.OffChain         (distributionTx, encsMintTx)
import           ENCOINS.ENCS.OnChain          (ENCSParams, distributionValidatorAddresses, encsTokenName, encsCurrencySymbol)
import           ENCS.Opts                     (ServerMode (Run, Setup, Verify), runWithOpts)
import           GHC.Generics                  (Generic)
import           Plutus.V2.Ledger.Api          (Address, TxOutRef)
import           PlutusAppsExtra.IO.ChainIndex (ChainIndex (..), HasChainIndex, getUnspentTxOutFromRef)
import           PlutusAppsExtra.IO.Wallet     (ownAddresses)
import           PlutusAppsExtra.Types.Error   (MkTxError (..))
import           PlutusAppsExtra.Utils.Address (addressToBech32)
import qualified PlutusTx.Prelude              as Plutus
import Control.Monad.Extra (whenM, unlessM)
import PlutusAppsExtra.IO.Blockfrost (getAssetHistory)
import PlutusAppsExtra.Utils.Blockfrost (AssetHistoryResponse(..), BfMintingPolarity (..))
import PlutusAppsExtra.Utils.Servant (handle404)

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

    serverSetup = setupH $ do
            ENCSEnv{..} <- asks envAuxiliary
            checkThatSetupUTXOExists $ fst envENCSParams
            let fee          = 100
                nFeeCovered  = Plutus.length envAddrList
                distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
            unless (null distribution) $ do
                let utxos = Just $ head distribution
                addrs <- ownAddresses
                void $ mkTx addrs def [encsMintTx envENCSParams utxos]
        where
            checkThatSetupUTXOExists txOutRef =
                whenM (isNothing <$> getUnspentTxOutFromRef txOutRef) $ throwM UTXOFromParamsDoesntExists
            setupH = handle $ \case
                UTXOFromParamsDoesntExists -> liftIO $ putStrLn "UTXO from encs-params doesn't exists."

    serverIdle = idleH $ do
        unlessM isTokensMinted $ throwM TokensHaveNotBeenMinted
        go
        where
            go = do
                ENCSEnv{..} <- asks envAuxiliary
                checkForCleanUtxos
                let fee          = 100
                    nFeeCovered  = Plutus.length envAddrList
                    distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
                    addrs = distributionValidatorAddresses distribution
                void $ mkTx addrs def $ map distributionTx $ init $ tails distribution
                go
            isTokensMinted = handle404 (pure False) $ do
                p <- asks (envENCSParams . envAuxiliary)
                history <- liftIO $ getAssetHistory (encsCurrencySymbol p) encsTokenName
                return $ any (\AssetHistoryResponse{..} -> ahrMintingPolarity == BfMint && ahrAmount == snd p) history
            idleH = (`catches` [Handler preH, Handler endH])
            endH e = case e of
                AllConstructorsFailed{} -> liftIO $ putStrLn "The distribution is completed!"
                _                       -> throwM e
            preH = \case
                TokensHaveNotBeenMinted -> liftIO $ putStrLn 
                    "ENCS tokens haven't been minted yet. You should run application with '--setup' flag first."

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

data SetupError
    = UTXOFromParamsDoesntExists
    deriving (Show, Exception)

data IdleError
    = TokensHaveNotBeenMinted
    deriving (Show, Exception)