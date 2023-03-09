{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module ENCS.Main where

import           Cardano.Api                      (writeFileJSON, TxMetadataJsonSchema (TxMetadataJsonNoSchema), metadataFromJson)
import           Cardano.Server.Config            (decodeOrErrorFromFile)
import           Cardano.Server.Internal          (AppM (..), Env (..), HasServer (..), getNetworkId, runAppM)
import           Cardano.Server.Tx                (checkForCleanUtxos, mkBalanceTx, mkTxErrorH)
import           Cardano.Server.Utils.Logger      (HasLogger(..), logPretty, logSmth)
import           Control.Monad                    (unless, void)
import           Control.Monad.Catch              (Exception (..), Handler (..), MonadThrow (throwM), catches, handle)
import           Control.Monad.Extra              (whenM)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Reader             (asks)
import           Data.Aeson                       (FromJSON (..), eitherDecodeFileStrict, toJSON, object, Value, (.=))
import           Data.Default                     (def)
import           Data.Either.Extra                (eitherToMaybe)
import           Data.Functor                     ((<&>))
import           Data.List                        (tails)
import           Data.Maybe                       (fromMaybe, isNothing)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           ENCOINS.ENCS.Distribution        (mkDistribution, processDistribution)
import           ENCOINS.ENCS.Distribution.IO     (verifyDistribution)
import           ENCOINS.ENCS.OffChain            (distributionTx, encsMintTx)
import           ENCOINS.ENCS.OnChain             (ENCSParams, distributionValidatorAddresses)
import           ENCS.Opts                        (ServerMode (Run, Setup, Verify), runWithOpts)
import           GHC.Generics                     (Generic)
import           Ledger.Ada                       (lovelaceValueOf)
import           Plutus.V2.Ledger.Api             (Address)
import           PlutusAppsExtra.Constraints.OffChain (utxoProducedTx)
import           PlutusAppsExtra.IO.ChainIndex    (ChainIndex (..), getUnspentTxOutFromRef)
import           PlutusAppsExtra.IO.Node          (sumbitTxToNodeLocal)
import           PlutusAppsExtra.IO.Wallet        (ownAddresses, signTx, getWalletAddr)
import           PlutusAppsExtra.Types.Error      (MkTxError (..))
import           PlutusAppsExtra.Utils.Address    (addressToBech32)
import           PlutusAppsExtra.Utils.Tx         (addMetadataToCardanoTx)
import qualified PlutusTx.Prelude                 as Plutus

encsMetadata :: Data.Aeson.Value
encsMetadata = object
  [
    "20" .= object
      [
        "123abc" .= object
          [
            "ENCS" .= object
            [
                "decimals" .= (6 :: Int),
                "desc" .= ("ENCOINS protocol" :: Text),
                "icon" .= ("ipfs://abcd" :: Text),
                "ticker" .= ("ENCS" :: Text),
                "url" .= ("https://encoins.io" :: Text),
                "version" .= ("1.0" :: Text)
            ]
          ]
      ]
  ]

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
            logSmth $ metadataFromJson TxMetadataJsonNoSchema $ toJSON encsMetadata
            ENCSEnv{..} <- asks envAuxiliary
            checkThatSetupUTXOExists $ fst envENCSParams
            let fee          = 100_000_000
                nFeeCovered  = Plutus.length envAddrList
                distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
            unless (null distribution) $ do
                let utxos = Just $ head distribution
                addrs <- ownAddresses
                void $ mkTxErrorH $ do
                    balancedTx <- mkBalanceTx addrs def [encsMintTx envENCSParams utxos]
                    let balancedTx' = addMetadataToCardanoTx balancedTx (eitherToMaybe $ metadataFromJson TxMetadataJsonNoSchema $ toJSON encsMetadata)
                    logPretty balancedTx'
                    -- logMsg "Signing..."
                    -- signedTx <- signTx balancedTx'
                    -- logPretty signedTx
                    -- logMsg "Submitting..."
                    -- networkId <- getNetworkId
                    -- node      <- asks envNodeFilePath
                    -- void $ liftIO $ sumbitTxToNodeLocal node networkId signedTx
                    -- logMsg "Submited."
                -- void $ mkTx addrs def [encsMintTx envENCSParams utxos]
        where
            checkThatSetupUTXOExists txOutRef =
                whenM (isNothing <$> getUnspentTxOutFromRef txOutRef) $ throwM UTXOFromParamsDoesntExists
            setupH = handle $ \case
                UTXOFromParamsDoesntExists -> liftIO $ putStrLn "UTXO from encs-params doesn't exists."

    serverIdle = idleH $ do
        -- unlessM isTokensMinted $ throwM TokensHaveNotBeenMinted
        go
        where
            go = do
                ENCSEnv{..} <- asks envAuxiliary
                walletAddr <- getWalletAddr
                checkForCleanUtxos
                let fee          = 100_000_000
                    nFeeCovered  = Plutus.length envAddrList
                    distribution = mkDistribution envENCSParams envAddrList (fee, nFeeCovered)
                    addrs = distributionValidatorAddresses distribution
                    m d = distributionTx d >> utxoProducedTx walletAddr (lovelaceValueOf 10_000_000) (Nothing :: Maybe ())
                void $ mkTxErrorH $ do
                    balancedTx <- mkBalanceTx addrs def $ map m $ init $ tails distribution
                    logPretty balancedTx
                    logMsg "Signing..."
                    signedTx <- signTx balancedTx
                    logPretty signedTx
                    logMsg "Submitting..."
                    networkId <- getNetworkId
                    node      <- asks envNodeFilePath
                    void $ liftIO $ sumbitTxToNodeLocal node networkId signedTx
                    logMsg "Submited."
                go
            -- isTokensMinted = handle404 (pure False) $ do
            --     p <- asks (envENCSParams . envAuxiliary)
            --     history <- liftIO $ getAssetHistory (encsCurrencySymbol p) encsTokenName
            --     return $ any (\AssetHistoryResponse{..} -> ahrMintingPolarity == BfMint && ahrAmount == snd p) history
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