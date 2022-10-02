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

import qualified Common.Utils           as U
import           YacadaNFT
import           YacadaCoin
--ON-CHAIN



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




mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do 
        now   <- currentTime
        let 
            yacada          = Value.singleton yacadaSymbol (U.yacadaName) (U.calculateYacada $ mpAdaAmount mp)
            yacadaNft       = Value.singleton yacadaNFTSymbol (U.giveReferralNFTName (mpAdaAmount mp) now)  1
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
            
            yacada          = Value.singleton yacadaSymbol (U.yacadaName) (U.calculateYacada $ mpAdaAmount mp)
            yacadaNft       = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName (mpAdaAmount mp) now)  1
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
























