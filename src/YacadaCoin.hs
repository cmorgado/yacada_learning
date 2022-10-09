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

module YacadaCoin 
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
import           Plutus.V1.Ledger.Bytes (getLedgerBytes)
import           Plutus.Script.Utils.V1.Scripts  
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified Common.Utils           as U


{-# INLINABLE yacadaPolicy #-}
yacadaPolicy ::  () -> PlutusV1.ScriptContext -> Bool
yacadaPolicy _ ctx  =   traceIfFalse "Not Minted" allOk 
                        &&  traceIfFalse "Wrong qt of yacadas" qt
       
    where        
        allOk :: Bool
        allOk = U.hashMinted (ownCurrencySymbol ctx) $ flattenValue (minted)
        
        info :: TxInfo
        info = scriptContextTxInfo ctx

        minted :: Value
        minted = txInfoMint info

        txOuts :: [TxOut]
        txOuts = txInfoOutputs info

        yacadasValue :: Integer
        yacadasValue = U.mintedQtOfValue (ownCurrencySymbol ctx) (flattenValue (minted)) 0

        qt :: Bool
        qt = yacadasValue == 1000



        -- ?? Paied amount ?? --
        
        -- ?? name of yacadaNFT == level given by amount of ADA && quantity == 1
        -- ?? did the treasury account received the ADA
        -- ?? did the referral NFT name correct quantity = 1??
        -- ?? did the referral received ADA and new NFT
                                                     

policy :: Scripts.MintingPolicy
policy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy yacadaPolicy ||])                                                                   

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














































































