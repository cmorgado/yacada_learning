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
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.Builtins
import qualified Common.UtilsV1           as U
import qualified Common.TypesV1           as T

{-# INLINABLE yacadaLevelPolicy #-}
yacadaLevelPolicy ::  BuiltinData -> PlutusV1.ScriptContext -> Bool
yacadaLevelPolicy redeemer' ctx =  validationIsOk    
    where

        mp :: T.MintParams
        mp = PlutusTx.unsafeFromBuiltinData @T.MintParams redeemer'    
        
        allOk :: Bool
        allOk = U.hashMinted (ownCurrencySymbol ctx) $ flattenValue (minted)

        minted :: Value
        minted = txInfoMint $ U.info ctx

        txOuts :: [TxOut]
        txOuts = txInfoOutputs $ U.info ctx

        yacadasNFTValue :: Integer
        yacadasNFTValue = U.mintedQtOfValue (ownCurrencySymbol ctx) (flattenValue (minted)) 0
        
       
        -- check if minted is inline with payed amount
        qt :: Bool
        qt = (yacadasNFTValue -1) == U.giveReferralNFTValue totalAdaInvested     

          -- check if minted is inline with payed amount
        qtNoReferral :: Bool
        qtNoReferral = (yacadasNFTValue -1) /= 0

        referralUtxos :: [TxOut]
        referralUtxos = T.referralTx mp

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
        referralAddr = pubKeyHashAddress (T.referral mp) Nothing
        -- NOTE: on final contract this addr has to be "hardcoded" to prevent hijack of treasury
        treasuryAddr :: Address
        treasuryAddr = pubKeyHashAddress (T.treasury mp) Nothing
         -- has referral      
        noReferral :: Bool
        noReferral = treasuryAddr == referralAddr
        -- get the ADA treasury will receive 
        treasuryAda :: Integer
        treasuryAda = U.sentAda ctx (T.treasury mp) 
        -- get the ADA rederral will receive
        referralAda :: Integer
        referralAda = U.sentAda ctx (T.referral mp)                  
          
        totalAdaInvested :: Integer
        totalAdaInvested  = referralAda + treasuryAda
                                          
        validationIsOk :: Bool
        validationIsOk = do 
            { let a = traceIfFalse "Referral not Minted" allOk
            ; let b = traceIfFalse "New Referal quantity" (qt)
            ; let c = traceIfFalse "referral Has Referral" (length referralUtxos >=1)
            ; let d = traceIfFalse "Referral Ratio " (referralCalculatedAdas == referralAda) 
            ; let e = traceIfFalse "Referral Ratio must be <50" (referralRatio < 50) 
            ; let f = traceIfFalse "Referral /= paying referral" (allReferencesFromReferrer referralUtxos)    
            ; let g = traceIfFalse "Referral NFT bad quantity" (qtNoReferral)
            ;   if noReferral then
                    traceIfFalse "Validation failed no referral" $ all(==(True::Bool)) [a,g]
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
                        void $ writeFileTextEnvelope "output/yacadaLevelNFT-policy-V1.plutus" Nothing yacadaNFTSerialisedScriptV1