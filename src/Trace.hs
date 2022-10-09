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

module Trace where
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Data.Default           (def)
import           Data.Map               as Map
import           Data.Maybe
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator  ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api                
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet 
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx.AssocMap
import qualified Common.Utils           as U
import           YacadaNFT
import           YacadaCoin
import           PlutusTx.Builtins
import           PlutusTx

    


data MintParams  = MintParams
    {  
        treasury :: !PaymentPubKeyHash,
        referral :: !PaymentPubKeyHash,
        mpAdaAmount :: !Integer     
    } deriving (Generic, ToJSON, FromJSON) 
PlutusTx.unstableMakeIsData ''MintParams 
-- OFF CHAIN    

fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,x,_) = x

-- 
getTot :: [Value] ->  [(CurrencySymbol, TokenName, Integer)] ->  [(CurrencySymbol, TokenName, Integer)]
getTot [] x = x
getTot (x:xs) y = do   
        let w = flattenValue  x
        let m = fst' $ head w
        if m == yacadaNFTSymbol
            then  getTot xs y ++ w
        else
            getTot xs y


extractLevel :: [(CurrencySymbol, TokenName, Integer)] -> Integer -> Integer
extractLevel [] v = v
extractLevel (x:xs) i = do
    let currSym = fst' x 
    let tokName = snd' x
    let tn = toString tokName
    if currSym == yacadaNFTSymbol        
        then            
            extractLevel xs $ ( U.referralLevel $ snd' x) + i
    else
        extractLevel xs i


-- ------------------------------------------------------------------------------------------------------------------
mintWithFriend :: MintParams -> Contract w FreeSchema Text ()
mintWithFriend mp = do 
        let  referralAddr       = pubKeyHashAddress (referral mp) Nothing
        now                     <- currentTime
        utxosReferral           <- utxosAt referralAddr         
        let
            vals                =  (_ciTxOutValue <$> (snd <$> Map.toList utxosReferral))            
            referralOk          =  extractLevel (getTot vals []) 0

            yacada              = Value.singleton yacadaSymbol (U.yacadaName) (U.calculateYacada $ mpAdaAmount mp)  -- coins for customer
            yacadaNft           = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName (mpAdaAmount mp) now)  1  -- NFT for is base referral
            yacadaReferralNft   = Value.singleton yacadaNFTSymbol  (U.upgradeReferralNFTName (referralOk+1) now)  1 -- upgrade for the referral account
            treasuryAdas        = Ada.lovelaceValueOf $ U.treasuryAda (mpAdaAmount mp) referralOk 
            referralAdas        = Ada.lovelaceValueOf $ U.referralAda (mpAdaAmount mp) referralOk            
            lookups             = Constraints.mintingPolicy policy 
                                    <> Constraints.mintingPolicy levelPolicy 
            payment             = Constraints.mustPayToPubKey (treasury mp) treasuryAdas 
                                    <> Constraints.mustPayToPubKey (referral mp) (referralAdas <>  yacadaReferralNft)                             
            mint                = Constraints.mustMintValueWithRedeemer (Redeemer { getRedeemer = (toBuiltinData mp)}) (yacada <> yacadaNft <> yacadaReferralNft)                              
            tx                  = mint <> payment
                                                            
       
        
        --logInfo @String $ printf "--------------------Referral---%s ------------------------\n" (show (U.hashMinted yacadaNFTSymbol ( getTot vals [])))
        logInfo @String $ printf "---------- %s ----------" (show  (treasury mp))
        logInfo @String $ printf "---------- %s ----------" (show referralAddr)
        --logInfo @String $ printf "---------- level %s ----------" (show referralOk)
        --logInfo @String $ printf "---------- vals %s ----------" $ show (getTot vals [])
        --logInfo @String $ printf "| UTXOs: %s |" (show utxosReferral)

        --logInfo @String $ printf "| VALUES: '%s' |\n"  (show $ getTot vals  []) --(show $ PlutusTx.AssocMap.keys $ Data.Maybe.fromJust $ PlutusTx.AssocMap.lookup "" (Plutus.V1.Ledger.Value.getValue $ head vals))
        logInfo @String $ printf "------------------------------------------------------"
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "------------------------------------------------------"
        --logInfo @String $ printf "| UTXOs: %s |" (show utxosReferral)
        --logInfo @String $ printf "------------------------------------------------------"
-----------------------------------------------------------------------------------------------------------------------
type FreeSchema = -- Endpoint "mint" MintParams .\/  
             Endpoint "mintWithFriend" MintParams 

endpoints :: Contract () FreeSchema Text ()
endpoints = awaitPromise (mintWithFriend') >> endpoints
  where
    mintWithFriend'    = endpoint @"mintWithFriend" mintWithFriend


wallet :: Integer -> Wallet
wallet = knownWallet

pkh :: Integer -> PaymentPubKeyHash
pkh x = mockWalletPaymentPubKeyHash $ wallet x

test:: IO ()
test= do
    
    let 
        dist = Map.fromList [ (wallet 1, 
                                Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName 200_000_000 100000000001)  1
                                <> Ada.lovelaceValueOf 1_000_000_000 ) -- treasury
                            , (wallet 2, Ada.lovelaceValueOf 1_000_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 200_000_000 
                                <> Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName 600_000_000 100000000002)  1)                                                                      
                            , (wallet 4, Ada.lovelaceValueOf 1_000_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 2_000_000_000)
                            , (wallet 6, Ada.lovelaceValueOf 1_000_000_000)
                            ]
        emCfg = EmulatorConfig (Left dist) def 
    runEmulatorTraceIO' def emCfg $ do                                                                                                                                                
                            h1 <- activateContractWallet (wallet 1) endpoints
                            h2 <- activateContractWallet (wallet 2) endpoints
                            h3 <- activateContractWallet (wallet 3) endpoints -- one referral
                            h4 <- activateContractWallet (wallet 4) endpoints
                            h5 <- activateContractWallet (wallet 5) endpoints
                            h6 <- activateContractWallet (wallet 6) endpoints
                            void $ Emulator.waitNSlots 1
                            callEndpoint @"mintWithFriend" h2 $ MintParams
                                                {
                                                    treasury = (pkh 1), 
                                                    referral= (pkh 3),                                                                                                                      
                                                    mpAdaAmount = 200_000_000
                                                }
                        --    void $ Emulator.waitNSlots 10
                        --    callEndpoint @"mintWithFriend" h4 $ MintParams -- 
                        --                  {     
                        --                    treasury = (pkh 1) , 
                        --                    referral=  (pkh 3),     
                        --                    mpAdaAmount = 400_000_000
                        --                  }
                        --    void $ Emulator.waitNSlots 10
                        --    callEndpoint @"mintWithFriend" h5 $ MintParams
                        --                {                                    
                        --                    treasury = (pkh 1) , 
                        --                    referral=  (pkh 3),
                        --                    mpAdaAmount = 1_000_000_001
                        --                }
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
























