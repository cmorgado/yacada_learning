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
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified Plutus.V2.Ledger.Contexts       as PlutusV2
import           Plutus.V1.Ledger.Bytes (getLedgerBytes)
import           Plutus.Script.Utils.V1.Scripts  
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified Common.UtilsV1           as U



data MintParams  = MintParams
    {  
        treasury :: PaymentPubKeyHash,
        referral :: PaymentPubKeyHash,
        referralTx :: [TxOut],
        mpAdaAmount :: Integer     
    } 
PlutusTx.unstableMakeIsData ''MintParams



{-# INLINABLE yacadaPolicy #-}
yacadaPolicy ::  BuiltinData -> PlutusV1.ScriptContext -> Bool
yacadaPolicy redeemer' ctx  =  traceIfFalse "Not Minted" allOk 
                        && traceIfFalse "Wrong qt of yacadas" qt
                        && traceIfFalse "Wrong amount paied to tresury or referral" adaMoved
                       
       
    where
        mp :: MintParams
        mp = PlutusTx.unsafeFromBuiltinData @MintParams redeemer'        
        
        -- check if YACADA was minted
        allOk :: Bool
        allOk = U.hashMinted (ownCurrencySymbol ctx) $ flattenValue (minted)

        -- get how many yacadas where minted
        yacadasValue :: Integer
        yacadasValue = U.mintedQtOfValue (ownCurrencySymbol ctx) (flattenValue (minted)) 0

        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- all Value minted
        minted :: Value
        minted = txInfoMint info

        txOuts :: [TxOut]
        txOuts = txInfoOutputs info

        -- base on thhe ADA paied and distributed to treasury and referral verify the amout of yacada minted
        qt :: Bool
        qt = yacadasValue == shouldReceiceYacada

        txInputs :: [TxInInfo]
        txInputs = txInfoInputs info
   
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
        treasuryAda = U.mintedQtOfValue Ada.adaSymbol (flattenValue(U.valuePaidToAddress ctx treasuryAddr)) 0
   
        -- get the ADA rederral will receive
        referralAda :: Integer
        referralAda = U.mintedQtOfValue Ada.adaSymbol (flattenValue(U.valuePaidToAddress ctx referralAddr)) 0

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
                        void $ writeFileTextEnvelope "yacada-policy-V1.plutus" Nothing yacadaSerialisedScriptV1














































































