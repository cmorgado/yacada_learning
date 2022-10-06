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
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import qualified Plutus.Script.Utils.V1.Scripts  as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as LedgerV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import           Plutus.V1.Ledger.Time  (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import           Plutus.V1.Ledger.Bytes (getLedgerBytes)                
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Common.Utils           as U

{-# INLINABLE yacadaLevelPolicy #-}
yacadaLevelPolicy ::  () -> PlutusV1.ScriptContext -> Bool
yacadaLevelPolicy _ ctx = traceIfFalse "LevelPolicy" allOk
    where
        allOk :: Bool
        allOk = True 
        
        info :: TxInfo
        info = scriptContextTxInfo ctx


levelPolicy :: Scripts.MintingPolicy
levelPolicy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy yacadaLevelPolicy ||]) 


-- Yacada NFT
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