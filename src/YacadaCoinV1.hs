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

module YacadaCoinV1 
    (
        yacadaSymbol,
        policy,
        yacadaWriteSerialisedScriptV1
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
import qualified Common.UtilsV1           as U
import qualified Common.TypesV1           as T


--treasury :: PlutusV2.PubKeyHash
--treasury = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [60, 47, 234, 52, 123, 28, 154, 240, 143, 240, 62, 109, 37, 231, 123, 240, 32, 118, 204, 101, 205, 133, 30, 131, 27, 182, 139, 132] }


{-# INLINABLE yacadaPolicy #-}
yacadaPolicy ::  BuiltinData -> PlutusV1.ScriptContext -> Bool
yacadaPolicy redeemer' ctx  =  validationIsOk       
    where
        mp :: T.MintParams
        mp = PlutusTx.unsafeFromBuiltinData @T.MintParams redeemer'        
        
        -- check if YACADA was minted
        allOk :: Bool
        allOk = U.hashMinted (ownCurrencySymbol ctx) $ flattenValue (minted)

        -- get how many yacadas where minted
        yacadasValue :: Integer
        yacadasValue = U.mintedQtOfValue (ownCurrencySymbol ctx) (flattenValue (minted)) 0

        -- all Value minted
        minted :: Value
        minted = txInfoMint $ U.info ctx

        txOuts :: [TxOut]
        txOuts = txInfoOutputs $ U.info ctx

        -- base on thhe ADA paied and distributed to treasury and referral verify the amout of yacada minted
        qt :: Bool
        qt = yacadasValue == shouldReceiceYacada

        qtNoReferral :: Bool
        qtNoReferral = yacadasValue == 0

        txInputs :: [TxInInfo]
        txInputs = txInfoInputs $ U.info ctx
   
        referralAddr :: Address
        referralAddr = pubKeyHashAddress (T.referral mp) Nothing

        -- NOTE: on final contract this addr has to be "hardcoded" to prevent hijack of treasury
        treasuryAddr :: Address
        treasuryAddr = pubKeyHashAddress (T.treasury mp) Nothing

        -- has referral      
        noReferral :: Bool
        noReferral = treasuryAddr == referralAddr

        -- should get the ADA from TX 
        shouldReceiceYacada :: Integer
        shouldReceiceYacada = U.calculateYacada (treasuryAda + referralAda)            

        treasuryAda :: Integer
        treasuryAda = U.sentAda ctx (T.treasury mp) 
        -- get the ADA rederral will receive
        referralAda :: Integer
        referralAda = U.sentAda ctx (T.referral mp)          

        adaMoved :: Bool
        adaMoved = referralAda + treasuryAda == (T.mpAdaAmount mp)
                                                                                              
        validationIsOk :: Bool
        validationIsOk = do 
            { let a = traceIfFalse "Not Minted" allOk 
            ; let b = traceIfFalse "Wrong qt of yacadas" (not qtNoReferral)
            ; let c = traceIfFalse "Wrong qt of yacadas" qt
            ; let d = traceIfFalse "Wrong amount paied to tresury or referral" adaMoved
            ;   if noReferral then
                    traceIfFalse "Validation failed no referral" $ all(==(True::Bool)) [a,b]
                else
                    traceIfFalse "Validation failed" $ all(==(True::Bool)) [a,c,d]
            }

policy :: Scripts.MintingPolicy
policy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])       
    where
        wrap = PSU.V1.mkUntypedMintingPolicy yacadaPolicy                                                            

-- Yacada Token
{-# INLINABLE yacadaSymbol #-}
yacadaSymbol :: CurrencySymbol
yacadaSymbol = Scripts.scriptCurrencySymbol policy

yacadaScriptV1 :: PlutusV1.Script
yacadaScriptV1 = PlutusV1.unMintingPolicyScript policy

yacadaScriptSBSV1 :: SBS.ShortByteString
yacadaScriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise yacadaScriptV1
             
yacadaSerialisedScriptV1 :: PlutusScript PlutusScriptV1
yacadaSerialisedScriptV1 = PlutusScriptSerialised yacadaScriptSBSV1

yacadaWriteSerialisedScriptV1 :: IO ()
yacadaWriteSerialisedScriptV1 = do
                        void $ writeFileTextEnvelope "output/yacada-policy-V1.plutus" Nothing yacadaSerialisedScriptV1


