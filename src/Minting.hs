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

module Minting where
import           Cardano.Api            (PlutusScriptV1,writeFileTextEnvelope)
import           Cardano.Api.Shelley    (PlutusScript (..))
import           Codec.Serialise
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import qualified Plutus.Script.Utils.V1.Scripts  as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
--import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as LedgerV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
--import qualified Plutus.V2.Ledger.Api                 as PlutusV2
--import qualified Plutus.V2.Ledger.Contexts            as PlutusV2

import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet




--ON-CHAIN

{-# INLINABLE freeMintingPolicy #-}
freeMintingPolicy :: () -> PlutusV1.ScriptContext -> Bool
freeMintingPolicy _ _ = True

policy :: Scripts.MintingPolicy
policy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy freeMintingPolicy ||]) --unTypedValidator


-- MIXED
curSymbol :: CurrencySymbol
curSymbol = Scripts.scriptCurrencySymbol  policy

scriptV1 :: PlutusV1.Script
scriptV1 = PlutusV1.unMintingPolicyScript policy

scriptSBSV1 :: SBS.ShortByteString
scriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1
              

serialisedScriptV1 :: PlutusScript PlutusScriptV1
serialisedScriptV1 = PlutusScriptSerialised scriptSBSV1

writeSerialisedScriptV1 :: IO ()
writeSerialisedScriptV1 = do
                        void $ writeFileTextEnvelope "token-name-policy-V1.plutus" Nothing serialisedScriptV1




-- OFF CHAIN    
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do 
          let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
              lookups = Constraints.mintingPolicy policy
              tx      = Constraints.mustMintValue val
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ printf "We forged %s" (show val)

type FreeSchema = Endpoint "mint" MintParams

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

test:: IO ()
test= runEmulatorTraceIO $ do
                          h1 <- activateContractWallet (knownWallet 1) endpoints
                          h2 <- activateContractWallet (knownWallet 2) endpoints
                          callEndpoint @"mint" h1 $ MintParams
                                                  { mpTokenName = "Batch43token",
                                                    mpAmount = 11000}
                          void $ Emulator.waitNSlots 10




























