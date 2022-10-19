{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module YacadaNFT 
    (
        yacadaNFTSymbol,
        levelPolicy,
        yacadaNFTWriteSerialisedScriptV2
    )
    where
import           Cardano.Api                          (PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.String
import           Ledger.Address
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Value               as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.Script.Utils.V2.Scripts       as UtilsScriptsV1 (scriptCurrencySymbol)
import           Plutus.V1.Ledger.Tx                  (TxId (getTxId))
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2  
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       print, (.))        
import qualified Common.UtilsV2          as U


data MintParams  = MintParams
    {  
        treasury :: PaymentPubKeyHash,
        referral :: PaymentPubKeyHash,
        referralTx :: [PlutusV2.TxOutRef],
        mpAdaAmount :: Integer     
    } 
PlutusTx.unstableMakeIsData ''MintParams

{-# INLINABLE yacadaLevelPolicy #-}
yacadaLevelPolicy ::  BuiltinData -> PlutusV2.ScriptContext -> Bool
yacadaLevelPolicy redeemer' ctx =   
    traceIfFalse "Yacada NFT not Minted" allOk
    && traceIfFalse "Yacada NFT quantity" qt

    where

        redeemer :: MintParams
        redeemer = PlutusTx.unsafeFromBuiltinData @MintParams redeemer'    
        
        allOk :: Bool
        allOk = U.hashMinted (PlutusV2.ownCurrencySymbol ctx) $ PlutusV1.flattenValue (minted)
 
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx
        -- all Value minted
        minted :: PlutusV1.Value
        minted = PlutusV2.txInfoMint info   

        txOuts :: [PlutusV2.TxOut]
        txOuts = PlutusV2.txInfoOutputs info       

        yacadasNFTValue :: Integer
        yacadasNFTValue = U.mintedQtOfValue (PlutusV2.ownCurrencySymbol ctx) (PlutusV1.flattenValue (minted)) 0
        
        qt :: Bool
        qt = yacadasNFTValue == 2                                         

        x :: [String]
        x = U.mintedTokenNames  (PlutusV1.flattenValue (minted))  []
      
        -- verify level minted
        -- verify sent to payer
        -- verify level upgrade to referral
        -- problem .. how to verify referral percentage with reference utxo (v2 upgrade)
      
      
levelPolicy :: Scripts.MintingPolicy
levelPolicy = PlutusV2.mkMintingPolicyScript
        $$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy yacadaLevelPolicy||])


-- Yacada NFT
{-# INLINABLE yacadaNFTSymbol #-}
yacadaNFTSymbol :: PlutusV2.CurrencySymbol
yacadaNFTSymbol = UtilsScriptsV1.scriptCurrencySymbol levelPolicy


yacadaNFTScriptV2 :: PlutusV2.Script
yacadaNFTScriptV2 = PlutusV2.unMintingPolicyScript levelPolicy

yacadaNFTScriptSBSV2 :: SBS.ShortByteString
yacadaNFTScriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise yacadaNFTScriptV2

yacadaNFTSerialisedScriptV2 :: PlutusScript PlutusScriptV2
yacadaNFTSerialisedScriptV2 = PlutusScriptSerialised yacadaNFTScriptSBSV2

yacadaNFTWriteSerialisedScriptV2 :: IO ()
yacadaNFTWriteSerialisedScriptV2 = do
                        void $ writeFileTextEnvelope "yacadaNFT-policy-V2.plutus" Nothing yacadaNFTSerialisedScriptV2









