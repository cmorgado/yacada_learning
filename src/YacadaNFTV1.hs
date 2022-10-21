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
{-# LANGUAGE NumericUnderscores  #-}

module YacadaNFTV1 
    (
        yacadaNFTSymbol,
        levelPolicy,
        yacadaNFTWriteSerialisedScriptV1
    )
    where
import           Cardano.Api            (PlutusScriptV1,writeFileTextEnvelope)
import           Cardano.Api.Shelley    (PlutusScript (..))
import           Codec.Serialise
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Hex
import           Data.String            (IsString (..))
import           Data.Void              (Void)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified PlutusTx
import           PlutusTx.Builtins.Class
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import           Ledger.Typed.Scripts.Validators
import qualified Plutus.Script.Utils.V1.Scripts  as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as LedgerV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import           Plutus.V1.Ledger.Bytes (getLedgerBytes)
import           Plutus.Script.Utils.V1.Scripts  
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.Builtins
import qualified Common.UtilsV1           as U

data MintParams  = MintParams
    {  
        treasury :: PaymentPubKeyHash,
        referral :: PaymentPubKeyHash,
        referralTx :: [TxOut],
        mpAdaAmount :: Integer     
    } 
PlutusTx.unstableMakeIsData ''MintParams

{-# INLINABLE yacadaLevelPolicy #-}
yacadaLevelPolicy ::  BuiltinData -> PlutusV1.ScriptContext -> Bool
yacadaLevelPolicy redeemer' ctx =  validationIsOk    
    where

        mp :: MintParams
        mp = PlutusTx.unsafeFromBuiltinData @MintParams redeemer'    
        
        allOk :: Bool
        allOk = U.hashMinted (ownCurrencySymbol ctx) $ flattenValue (minted)
 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        minted :: Value
        minted = txInfoMint info

        txOuts :: [TxOut]
        txOuts = txInfoOutputs info

        yacadasNFTValue :: Integer
        yacadasNFTValue = U.mintedQtOfValue (ownCurrencySymbol ctx) (flattenValue (minted)) 0
        
       
        -- check if minted is inline with payed amount
        qt :: Bool
        qt = (yacadasNFTValue -1) == U.giveReferralNFTValue totalAdaInvested     

        referralUtxos :: [TxOut]
        referralUtxos = referralTx mp

        -- gets the percentage for the referral  , if referal is the owner of nft    V2 should use the reference inputs instead of the redeemer   
        allReferralValues :: [TxOut] -> [(CurrencySymbol, TokenName, Integer)] -> [(CurrencySymbol, TokenName, Integer)]
        allReferralValues [] val = val
        allReferralValues (x:xs) val = allReferralValues xs ((flattenValue $ txOutValue x) ++ val)                    
        
        allReferencesFromReferrer :: [TxOut] -> Bool 
        allReferencesFromReferrer m = not $ any (\ x -> (txOutAddress x) /=  referralAddr  ) m
        
        referralRatio ::  Integer
        referralRatio =  U.mintedQtOfValue (ownCurrencySymbol ctx) (allReferralValues referralUtxos [] ) 0

        -- Calculates if the value paid to referral is correct inline with his YACADA_REFERRAL      
        -- V      
        referralCalculatedAdas :: Integer
        referralCalculatedAdas = divideInteger (multiplyInteger 1000 (multiplyInteger totalAdaInvested referralRatio)) 100_000

        referralAddr :: Address
        referralAddr = pubKeyHashAddress (referral mp) Nothing
        -- NOTE: on final contract this addr has to be "hardcoded" to prevent hijack of treasury
        treasuryAddr :: Address
        treasuryAddr = pubKeyHashAddress (treasury mp) Nothing
         -- has referral      
        noReferral :: Bool
        noReferral = treasuryAddr == referralAddr
        -- get the ADA treasury will receive 
        treasuryAda :: Integer
        treasuryAda = U.mintedQtOfValue Ada.adaSymbol (flattenValue(U.valuePaidToAddress ctx treasuryAddr)) 0
        -- get the ADA rederral will receive
        referralAda :: Integer
        referralAda = U.mintedQtOfValue Ada.adaSymbol (flattenValue(U.valuePaidToAddress ctx referralAddr)) 0                    
          
        totalAdaInvested :: Integer
        totalAdaInvested  = referralAda + treasuryAda
                                          
        validationIsOk :: Bool
        validationIsOk = do 
            { let a = traceIfFalse "Yacada NFT not Minted" allOk
            ; let b = traceIfFalse "Yacada New Referal quantity" (qt)
            ; let c = traceIfFalse "referral Has Referral" (length referralUtxos >=1)
            ; let d = traceIfFalse "Referral Ratio " (referralCalculatedAdas == referralAda) 
            ; let e = traceIfFalse "Referral Ratio must be <50" (referralRatio < 50) 
            ; let f = traceIfFalse "Referral /= paying referral" (allReferencesFromReferrer referralUtxos)    
            ;   if noReferral then
                    traceIfFalse "Validation failed no referral" $ all(==(True::Bool)) [a]
                else
                    traceIfFalse "Validation failed" $ all(==(True::Bool)) [a,b,c,d,e,f]
            }                                                                                   
      

levelPolicy :: Scripts.MintingPolicy
levelPolicy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy yacadaLevelPolicy ||]) 


-- Yacada NFT
{-# INLINABLE yacadaNFTSymbol #-}
yacadaNFTSymbol :: CurrencySymbol
yacadaNFTSymbol = Scripts.scriptCurrencySymbol levelPolicy

yacadaNFTScriptV1 :: PlutusV1.Script
yacadaNFTScriptV1 = PlutusV1.unMintingPolicyScript levelPolicy

yacadaNFTScriptSBSV1 :: SBS.ShortByteString
yacadaNFTScriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise yacadaNFTScriptV1
             
yacadaNFTSerialisedScriptV1 :: PlutusScript PlutusScriptV1
yacadaNFTSerialisedScriptV1 = PlutusScriptSerialised yacadaNFTScriptSBSV1

yacadaNFTWriteSerialisedScriptV1 :: IO ()
yacadaNFTWriteSerialisedScriptV1 = do
                        void $ writeFileTextEnvelope "yacadaLevelNFT-policy-V1.plutus" Nothing yacadaNFTSerialisedScriptV1