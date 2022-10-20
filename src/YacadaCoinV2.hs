{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module YacadaCoinV2 
    (
        yacadaSymbol,
        policy,
        yacadaWriteSerialisedScriptV2
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



{-# INLINABLE yacadaPolicy #-}
yacadaPolicy ::  BuiltinData -> PlutusV2.ScriptContext -> Bool
yacadaPolicy redeemer' ctx  =  traceIfFalse "Not Minted" allOk 
                        && traceIfFalse "Wrong qt of yacadas" qt
                        && traceIfFalse "Wrong amount paied to tresury or referral" adaMoved
                        -- &&  traceIfFalse "payed not ok" w
       
    where
        mp :: MintParams
        mp = PlutusTx.unsafeFromBuiltinData @MintParams redeemer'        
        
        -- check if YACADA was minted
        allOk :: Bool
        allOk = U.hashMinted (PlutusV2.ownCurrencySymbol ctx) $ PlutusV1.flattenValue (minted)

        -- get how many yacadas where minted
        yacadasValue :: Integer
        yacadasValue = U.mintedQtOfValue (PlutusV2.ownCurrencySymbol ctx) (PlutusV1.flattenValue (minted)) 0

        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        -- all Value minted
        minted :: PlutusV1.Value
        minted = PlutusV2.txInfoMint info

        txOuts :: [PlutusV2.TxOut]
        txOuts = PlutusV2.txInfoOutputs info
        
        txInsRefs :: [PlutusV2.TxInInfo]
        txInsRefs = PlutusV2.txInfoReferenceInputs  info

        -- base on thhe ADA paied and distributed to treasury and referral verify the amout of yacada minted
        qt :: Bool
        qt = yacadasValue == shouldReceiceYacada

        referralAddr :: Address
        referralAddr = pubKeyHashAddress (referral mp) Nothing

        -- NOTE: on final contract this addr has to be "hardcoded" to prevent hijack of treasury
        treasuryAddr :: Address
        treasuryAddr = pubKeyHashAddress (treasury mp) Nothing
      
        -- should get the ADA from TX 
        shouldReceiceYacada :: Integer
        shouldReceiceYacada = calculateYacada (treasuryAda + referralAda)            

        -- get the ADA treasury will receive 
        treasuryAda :: Integer
        treasuryAda = U.mintedQtOfValue PlutusV2.adaSymbol (PlutusV1.flattenValue(U.valuePaidToAddress ctx treasuryAddr)) 0
   
        -- get the ADA rederral will receive
        referralAda :: Integer
        referralAda = U.mintedQtOfValue PlutusV2.adaSymbol (PlutusV1.flattenValue(U.valuePaidToAddress ctx referralAddr)) 0

        adaMoved :: Bool
        adaMoved = referralAda + treasuryAda == (mpAdaAmount mp)

        calculateYacada :: Integer -> Integer
        calculateYacada ada 
            | ada == 200_000_000     =  1000
            | ada == 400_000_000     =  2040 -- minting bonus 10%
            | ada == 600_000_000     =  3090 -- minting bonus 15%
            | ada == 800_000_000     =  4160 -- minting bonus 20%
            | ada == 1000_000_000    =  5250 -- minting bonus 15%
            | otherwise              =  0                                   
      
        
        
        

        -- ?? name of yacadaNFT == level given by amount of ADA && quantity == 1
        
        -- ?? did the referral NFT name correct quantity = 1??
        -- ?? did the referral received ADA and new NFT
                                                     
policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript
        $$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy yacadaPolicy||])

-- Yacada Token
{-# INLINABLE yacadaSymbol #-}
yacadaSymbol :: PlutusV2.CurrencySymbol
yacadaSymbol = UtilsScriptsV1.scriptCurrencySymbol policy

yacadaScriptV2 :: PlutusV2.Script
yacadaScriptV2 = PlutusV2.unMintingPolicyScript policy

yacadaScriptSBSV2 :: SBS.ShortByteString
yacadaScriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise yacadaScriptV2

yacadaSerialisedScriptV2 :: PlutusScript PlutusScriptV2
yacadaSerialisedScriptV2 = PlutusScriptSerialised yacadaScriptSBSV2

yacadaWriteSerialisedScriptV2 :: IO ()
yacadaWriteSerialisedScriptV2 = do
                        void $ writeFileTextEnvelope "yacada-policy-V2.plutus" Nothing yacadaSerialisedScriptV2














































































