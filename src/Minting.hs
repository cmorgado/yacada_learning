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



--ON-CHAIN

{-# INLINABLE yacadaLevelPolicy #-}
yacadaLevelPolicy ::  () -> PlutusV1.ScriptContext -> Bool
yacadaLevelPolicy _ ctx = True

levelPolicy :: Scripts.MintingPolicy
levelPolicy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy yacadaLevelPolicy ||]) 

{-# INLINABLE yacadaPolicy #-}
yacadaPolicy ::  () -> PlutusV1.ScriptContext -> Bool
yacadaPolicy _ ctx = traceIfFalse "Just" signedByBeneficiary

    where
        signedByBeneficiary :: Bool
        signedByBeneficiary = True

policy :: Scripts.MintingPolicy
policy = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy yacadaPolicy ||]) 
                                 
                                      

-- Yacada Token
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


-- caculation of yacada and naming the NFT

calculateYacada :: Integer -> Integer
calculateYacada ada = case ada of 
    200     ->  1000
    400     ->  2000
    600     ->  3000
    800     ->  4000
    1000    ->  5000
    _       ->  0


giveReferralNFTName :: Integer -> POSIXTime -> TokenName 
giveReferralNFTName ada time = case ada of 
        200     ->  toTokenName ( "YACADA_REFERRAL_L01_"++ show(getPOSIXTime time)) -- getPOSIXTime
        400     ->  toTokenName ("YACADA_REFERRAL_L02_" ++  show(getPOSIXTime time))
        600     ->  toTokenName ("YACADA_REFERRAL_L03_" ++  show(getPOSIXTime time))
        800     ->  toTokenName ("YACADA_REFERRAL_L04_" ++  show(getPOSIXTime time))
        1000    ->  toTokenName ("YACADA_REFERRAL_L05_" ++  show(getPOSIXTime time))
        _       ->  toTokenName ("Error")

toTokenName :: String -> TokenName
toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn }

-- OFF CHAIN    
data MintParams = MintParams
    {  
        paymentTo :: !AdaDestinations ,
        mpAdaAmount :: !Integer,
        mpTime :: !POSIXTime
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

data AdaDestinations = AdaDestinations
    {
        treasury :: !PaymentPubKeyHash,
        referral :: !PaymentPubKeyHash
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

yacadaName :: TokenName
yacadaName = "YACADA_TOKEN"

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do 
        now   <- currentTime
        let 
            yacada          = Value.singleton yacadaSymbol yacadaName (calculateYacada $ mpAdaAmount mp)
            yacadaNft       = Value.singleton yacadaNFTSymbol (giveReferralNFTName (mpAdaAmount mp) now)  1
            adas            = Ada.lovelaceValueOf $ mpAdaAmount mp            
            lookups         = Constraints.mintingPolicy policy <> Constraints.mintingPolicy levelPolicy
            destinations    = paymentTo mp
            payment         = Constraints.mustPayToPubKey (treasury destinations) adas
            mintYacada      = Constraints.mustMintValue yacada <> Constraints.mustMintValue yacadaNft                                                 
            tx              = mintYacada
                                                             
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "------------------------------------------------------"
        logInfo @String $ printf "We forged y:%s NFT:%s" (show yacada)  (show yacadaNft)
        logInfo @String $ printf "------------------------------------------------------"
-- --------------------------------------------------------------------------------------------------------------------
mintWithFriend :: MintParams -> Contract w FreeSchema Text ()
mintWithFriend mp = do 
        let  destinations = paymentTo mp
             destinationAdress = pubKeyHashAddress (referral destinations) Nothing
        utxos   <- utxosAt destinationAdress
        now   <- currentTime
        let 
            
            yacada          = Value.singleton yacadaSymbol yacadaName (calculateYacada $ mpAdaAmount mp)
            yacadaNft       = Value.singleton yacadaNFTSymbol  (giveReferralNFTName (mpAdaAmount mp) now)  1
            adas            = Ada.lovelaceValueOf $ mpAdaAmount mp            
            lookups         = Constraints.mintingPolicy policy <> Constraints.mintingPolicy levelPolicy
            payment         = Constraints.mustPayToPubKey (treasury destinations) adas
            mintYacada      = Constraints.mustMintValue yacada <> Constraints.mustMintValue yacadaNft                          
            tx              = mintYacada
                                                             
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "------------------------------------------------------"
        logInfo @String $ printf "| ADRRESS:%s UTXOs: %s |" (show destinationAdress) (show utxos)
        logInfo @String $ printf "------------------------------------------------------"
-----------------------------------------------------------------------------------------------------------------------
type FreeSchema = Endpoint "mint" MintParams
            .\/   Endpoint "mintWithFriend" MintParams 

endpoints :: Contract () FreeSchema Text ()
endpoints = awaitPromise (mint' `select` mintWithFriend') >> endpoints
  where
    mint'               = endpoint @"mint" mint
    mintWithFriend'    = endpoint @"mintWithFriend" mintWithFriend


mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

test:: IO ()
test= runEmulatorTraceIO $ do
                          let w1 = knownWallet 1
                              w2 = knownWallet 2
                              w3 = knownWallet 3
                              w4 = knownWallet 4
                              ref = mockWalletPaymentPubKeyHash w1
                          h1 <- activateContractWallet (w1) endpoints
                          h2 <- activateContractWallet (w2) endpoints
                          h3 <- activateContractWallet (w3) endpoints
                          treasury <- activateContractWallet (w4) endpoints
                          callEndpoint @"mint" h1 $ MintParams
                                                  {
                                                    paymentTo = AdaDestinations { treasury = (mockWalletPaymentPubKeyHash w4), referral= (mockWalletPaymentPubKeyHash w2)},
                                                    mpAdaAmount = 200,
                                                    mpTime = 1664441716
                                                    }

                          void $ Emulator.waitNSlots 10
                          callEndpoint @"mintWithFriend" h2 $ MintParams
                                            {                                             
                                                paymentTo = AdaDestinations { treasury = (mockWalletPaymentPubKeyHash w4) , referral=  (mockWalletPaymentPubKeyHash w3)},
                                                mpAdaAmount = 400,
                                                mpTime = 1664441720
                                            }
























