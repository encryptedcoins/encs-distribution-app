{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module ENCS.Main where

import           Cardano.Api                          (writeFileJSON)
import           Cardano.Server.Config                (decodeOrErrorFromFile)
import           Cardano.Server.Internal              (AppM (..), Env (..), HasServer (..), getNetworkId, runAppM)
import           Cardano.Server.Tx                    (checkForCleanUtxos, mkBalanceTx, mkTx, mkTxErrorH)
import           Cardano.Server.Utils.Logger          (HasLogger (logMsg), logPretty, (.<))
import           Control.Monad                        (unless, void)
import           Control.Monad.Catch                  (Exception (..), Handler (..), MonadThrow (throwM), SomeException, catches,
                                                       handle)
import           Control.Monad.Extra                  (whenM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Reader                 (asks)
import           Data.Aeson                           (FromJSON (..), eitherDecodeFileStrict, (.=))
import qualified Data.Aeson                           as J
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Default                         (def)
import           Data.Either                          (partitionEithers)
import           Data.Functor                         ((<&>))
import           Data.List                            (tails, (\\))
import           Data.Maybe                           (fromJust, isNothing)
import           Data.String                          (IsString (fromString))
import           ENCOINS.ENCS.Distribution            (mkDistribution, processDistribution)
import           ENCOINS.ENCS.Distribution.IO         (verifyDistribution)
import           ENCOINS.ENCS.OffChain                (distributionTx, encsMintTx)
import           ENCOINS.ENCS.OnChain                 (ENCSParams, distributionValidatorAddresses)
import           ENCS.Opts                            (ServerMode (..), runWithOpts)
import           GHC.Generics                         (Generic)
import           Ledger                               (Slot)
import           Ledger.Ada                           (lovelaceValueOf)
import           Plutus.V2.Ledger.Api                 (Address, TokenName (TokenName))
import           PlutusAppsExtra.Constraints.OffChain (utxoProducedTx)
import           PlutusAppsExtra.IO.ChainIndex        (ChainIndex (..), getUnspentTxOutFromRef)
import           PlutusAppsExtra.IO.ChainIndex.Kupo   (getUtxosWithTokensBetweenSlots)
import           PlutusAppsExtra.IO.Node              (sumbitTxToNodeLocal)
import           PlutusAppsExtra.IO.Wallet            (getWalletAddr, ownAddresses, signTx)
import           PlutusAppsExtra.Types.Error          (MkTxError (..))
import           PlutusAppsExtra.Utils.Address        (addressToBech32, bech32ToAddress)
import qualified PlutusTx.Prelude                     as Plutus
import           System.Directory                     (removeFile)

runENCSApp :: IO ()
runENCSApp = do
    mode <- runWithOpts
    runAppM @ENCSApp $ case mode of
        Run            -> serverIdle
        Setup          -> serverSetup
        Verify from to -> verify from to
        Find   from to -> find   from to 

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
            let fee          = 100_000_000
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
        encsParams     <- asks $ envENCSParams . envAuxiliary
        newtworkId     <- getNetworkId
        distribution   <- getDistribution
        interruptedRes <- getInterruptedRes
        let toBech = fromJust . addressToBech32 newtworkId
            processedDistribution = map (\(a,b,_) -> (a,b)) interruptedRes
        res <- fmap (fmap (fmap $ \(a,b,c) -> (toBech a,b,c))) 
            $ addInterrupted interruptedRes $ liftIO $ verifyDistribution 
                newtworkId
                encsParams
                (distribution \\ processedDistribution)
                (saveIntermidiate newtworkId)
                interruptedRes
        liftIO $ void $ case partitionEithers res of
            ([], r) -> writeFileJSON to r
            (l , r) -> writeFileJSON to $ J.object 
                [ "verified" .= r
                , "failed"   .= map toBech l 
                ]
        liftIO $ removeFile toTemp
    where
        getDistribution = liftIO $ eitherDecodeFileStrict from >>= either fail (pure . processDistribution)
        getInterruptedRes = liftIO $ handle (\(_ :: SomeException) -> pure []) $ do
            file <- init <$> readFile toTemp
            case J.eitherDecode $ fromString $ '[' : file <> "]" of
                Right res -> pure $ map (\(a,b,c) -> (fromJust $ bech32ToAddress a,b,c)) res
                Left err -> [] <$ logMsg ("Can not parse previous interrupted distribution:\n" .< err)
        toTemp = let (name, ext) = break (== '.') to in name <> "_temp" <> ext
        saveIntermidiate newtworkId = Just $ mapM_ $ \tr -> do
            BS.appendFile toTemp . BSL.toStrict $ encodeIntermidiate newtworkId tr
            BS.appendFile toTemp ","
        encodeIntermidiate newtworkId tr@(addr, amount, txId) = maybe (J.encode tr) (J.encode . (,amount,txId)) $
            addressToBech32 newtworkId addr
        addInterrupted interrupted current = do
            current' <- current
            pure $ map Right interrupted <> current'

find :: Slot -> Slot -> AppM ENCSApp ()
find from to = do
    (_, amt) <- asks $ envENCSParams . envAuxiliary
    let name = TokenName "ENCS"
    void $ liftIO $ getUtxosWithTokensBetweenSlots name amt from to >>= writeFileJSON "find.json" 
    
data SetupError
    = UTXOFromParamsDoesntExists
    deriving (Show, Exception)

data IdleError
    = TokensHaveNotBeenMinted
    deriving (Show, Exception)