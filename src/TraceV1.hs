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

module TraceV1 where
import              Control.Monad                      hiding (fmap)
import              Data.Aeson                         (ToJSON, FromJSON)
import              Data.Text                          (Text)
import              Data.Void                          (Void)
import              Data.Default                       (def)
import              Data.Map                           as Map
import              Data.Maybe         
import              GHC.Generics                       (Generic)
import              Plutus.Contract                    as Contract
import              Plutus.Trace.Emulator              as Emulator  ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import              Ledger                             hiding (mint, singleton)
import              Ledger.Constraints                 as Constraints
import              Ledger.Value                       as Value
import              Ledger.Ada                         as Ada
import              Plutus.V1.Ledger.Value
import              Plutus.V1.Ledger.Api    
import              Playground.Contract                (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import              Playground.TH                      (mkKnownCurrencies, mkSchemaDefinitions)
import              Playground.Types                   (KnownCurrency (..))
import              Prelude                            (IO, Show (..), String, Semigroup (..))
import              Text.Printf                        (printf)
import              Wallet.Emulator.Wallet 
import              PlutusTx.Prelude                   hiding (Semigroup(..), unless)
import              PlutusTx.AssocMap
import qualified    Common.UtilsV1                       as U
import              YacadaNFTV1
import              YacadaCoinV1
import              PlutusTx.Builtins
import              PlutusTx


data MintParams  = MintParams
    {  
        treasury :: !PaymentPubKeyHash,
        referral :: !PaymentPubKeyHash,
        referralTx :: [TxOut], 
        mpAdaAmount :: !Integer
        
    } deriving (Generic, ToJSON, FromJSON) 
PlutusTx.unstableMakeIsData ''MintParams 
-- OFF CHAIN    
-- 
getTot :: [Value] ->  [(CurrencySymbol, TokenName, Integer)] ->  [(CurrencySymbol, TokenName, Integer)]
getTot [] x = x
getTot (x:xs) y = do   
        let w = flattenValue  x
        let m = U.fst' $ head w
        if m == yacadaNFTSymbol
            then  getTot xs (y ++ w)
        else
            getTot xs y


extractLevel :: [(CurrencySymbol, TokenName, Integer)] -> Integer -> Integer
extractLevel [] v = v
extractLevel (x:xs) i = do
    let currSym = U.fst' x 
    if currSym == yacadaNFTSymbol        
        then            
            extractLevel xs $ ( U.trd' x) + i
    else
        extractLevel xs i

getUtxosWithYacadaNFT ::  [(TxOutRef, ChainIndexTxOut)] ->  [TxOut] ->  [TxOut]
getUtxosWithYacadaNFT [] o = o
getUtxosWithYacadaNFT (x:xs) t  = do
    let ci = snd (x)
    let ciVal = flattenValue $ _ciTxOutValue ci
    let filtered = U.hashMinted yacadaNFTSymbol ciVal
    if filtered == True
        then
            getUtxosWithYacadaNFT xs ( [(toTxOut ci)] ++ t)
    else 
        getUtxosWithYacadaNFT xs t     
-- ------------------------------------------------------------------------------------------------------------------
mintWithFriend :: MintParams -> Contract w FreeSchema Text ()
mintWithFriend mp = do 
        let  referralAddr       = pubKeyHashAddress (referral mp) Nothing
        now                     <- currentTime
        utxosReferral           <- utxosAt referralAddr        
        let
            vals                = (_ciTxOutValue <$> (snd <$> Map.toList utxosReferral))            
            referralOk          = extractLevel (getTot vals []) 0
            refFilterdUtxos     = getUtxosWithYacadaNFT  (Map.toList utxosReferral ) []  -- Filtered UXTOs from referral to send as input references to validate on-chain

            yacada              = Value.singleton yacadaSymbol (U.yacadaName) (U.calculateYacada $ mpAdaAmount mp)  -- coins for customer
            yacadaNft           = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName)  (U.giveReferralNFTValue (mpAdaAmount mp))  -- NFT for is base referral
            yacadaReferralNft   = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName)   1 -- upgrade for the referral account
            treasuryAdas        = Ada.lovelaceValueOf $ U.treasuryAda (mpAdaAmount mp) referralOk 
            referralAdas        = Ada.lovelaceValueOf $ U.referralAda (mpAdaAmount mp) referralOk            
            lookups             = Constraints.plutusV1MintingPolicy policy 
                                    <> Constraints.plutusV1MintingPolicy levelPolicy 
            payment             = Constraints.mustPayToPubKey (treasury mp) treasuryAdas 
                                    <> Constraints.mustPayToPubKey (referral mp) (referralAdas <>  yacadaReferralNft)                             
            mint                = Constraints.mustMintValueWithRedeemer (Redeemer { getRedeemer = (toBuiltinData 
                                MintParams
                                {
                                    treasury = treasury mp,
                                    referral = referral mp,
                                    referralTx = refFilterdUtxos,
                                    mpAdaAmount = mpAdaAmount mp
                                })}) (yacada <> yacadaNft <> yacadaReferralNft)                              
            tx                  = mint <> payment
                                                            
       
        logInfo @String $ printf "------------------------------------------------------"
        logInfo @String $ printf "--------------------Referral---%s ------------------------\n" (show (U.hashMinted yacadaNFTSymbol ( getTot vals [])))
        --logInfo @String $ printf "---------- %s ----------" (show  (treasury mp))
        --logInfo @String $ printf "---------- %s ----------" (show referralAddr)
        logInfo @String $ printf "---------- level %s ----------" (show referralOk)
        --logInfo @String $ printf "---------- vals %s ----------" $ show (getTot vals [])
        --logInfo @String $ printf "---------- vals %s ----------" $ (show vals )
        --logInfo @String $ printf "| UTXOs: %s |" (show $ getUtxosWithYacadaNFT  (Map.toList utxosReferral ) [])

        --logInfo @String $ printf "| VALUES: '%s' |\n"  (show $ getTot vals  []) --(show $ PlutusTx.AssocMap.keys $ Data.Maybe.fromJust $ PlutusTx.AssocMap.lookup "" (Plutus.V1.Ledger.Value.getValue $ head vals))
        logInfo @String $ printf "------------------------------------------------------"
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "------------------------------------------------------"
        --logInfo @String $ printf "| UTXOs: %s |" (show utxosReferral)
        --logInfo @String $ printf "------------------------------------------------------"
-----------------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------------
mintAlone :: MintParams -> Contract w FreeSchema Text ()
mintAlone mp = do 
        let  referralAddr       = pubKeyHashAddress (referral mp) Nothing
        now                     <- currentTime
        utxosReferral           <- utxosAt referralAddr        
        let
            vals                = (_ciTxOutValue <$> (snd <$> Map.toList utxosReferral))                    
            refFilterdUtxos     = getUtxosWithYacadaNFT  (Map.toList utxosReferral ) []  -- Filtered UXTOs from referral to s

            yacada              = Value.singleton yacadaSymbol (U.yacadaName) (U.calculateYacada $ mpAdaAmount mp)  -- coins 
            yacadaNft           = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName)  (U.giveReferralNFTValue (mpAdaAmount mp))  -- NFT for is base referral
            yacadaReferralNft   = Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName)   1 -- upgrade for the referral ac
            treasuryAdas        = Ada.lovelaceValueOf $ U.treasuryAda (mpAdaAmount mp) 50 
            referralAdas        = Ada.lovelaceValueOf $ U.treasuryAda (mpAdaAmount mp) 50            
            lookups             = Constraints.plutusV1MintingPolicy policy 
                                    <> Constraints.plutusV1MintingPolicy levelPolicy 
            payment             = Constraints.mustPayToPubKey (treasury mp) treasuryAdas 
                                    <> Constraints.mustPayToPubKey (referral mp) (referralAdas <>  yacadaReferralNft)        
            mint                = Constraints.mustMintValueWithRedeemer (Redeemer { getRedeemer = (toBuiltinData 
                                MintParams
                                {
                                    treasury = treasury mp,
                                    referral = referral mp,
                                    referralTx = refFilterdUtxos,
                                    mpAdaAmount = mpAdaAmount mp
                                })}) (yacada <> yacadaNft <> yacadaReferralNft)                              
            tx                  = mint <> payment
                                                            
       
        logInfo @String $ printf "------------------------------------------------------"
        -- logInfo @String $ printf "--------------------Referral---%s ------------------------\n" (show (U.hashMinted yacadaNFTSymbol ( getTot vals [])))
        -- logInfo @String $ printf "---------- %s ----------" (show  (treasury mp))
        -- logInfo @String $ printf "---------- %s ----------" (show referralAddr)        
        logInfo @String $ printf "---------- treasuryAda %s ----------" (show (U.treasuryAda (mpAdaAmount mp) 50))
        logInfo @String $ printf "---------- referralAda %s ----------" (show (U.treasuryAda (mpAdaAmount mp) 50))
        logInfo @String $ printf "---------- yacadas %s ----------" (show ((U.calculateYacada $ mpAdaAmount mp)))
        -- logInfo @String $ printf "---------- vals %s ----------" $ show (getTot vals [])
        -- logInfo @String $ printf "---------- vals %s ----------" $ (show vals )
        -- logInfo @String $ printf "| UTXOs: %s |" (show $ getUtxosWithYacadaNFT  (Map.toList utxosReferral ) [])

        -- logInfo @String $ printf "| VALUES: '%s' |\n"  (show $ getTot vals  []) --(show $ PlutusTx.AssocMap.keys $ Data.May
        logInfo @String $ printf "------------------------------------------------------"
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        logInfo @String $ printf "------------------------------------------------------"
        --logInfo @String $ printf "| UTXOs: %s |" (show utxosReferral)
        --logInfo @String $ printf "------------------------------------------------------"
-----------------------------------------------------------------------------------------------------------------------
type FreeSchema =  Endpoint "mintAlone" MintParams .\/  
             Endpoint "mintWithFriend" MintParams 

endpoints :: Contract () FreeSchema Text ()
endpoints = awaitPromise (mintWithFriend' `select` mintAlone') >> endpoints
  where
    mintWithFriend'     = endpoint @"mintWithFriend" mintWithFriend
    mintAlone'          = endpoint @"mintAlone" mintAlone


wallet :: Integer -> Wallet
wallet = knownWallet

pkh :: Integer -> PaymentPubKeyHash
pkh x = mockWalletPaymentPubKeyHash $ wallet x

test:: IO ()
test= do
    
    let 
        dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 1_000_000_000 ) -- treasury
                            , (wallet 2, Ada.lovelaceValueOf 1_000_000_000  
                                <> Value.singleton yacadaNFTSymbol  (U.giveReferralNFTName )  15) -- referral
                            , (wallet 3, Ada.lovelaceValueOf 2_000_000_000)                                                                      
                            , (wallet 4, Ada.lovelaceValueOf 2_000_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 2_000_000_000)
                          
                            ]
        emCfg = EmulatorConfig (Left dist) def 
    runEmulatorTraceIO' def emCfg $ do                                                                                                                                                
                            h1 <- activateContractWallet (wallet 1) endpoints
                            h2 <- activateContractWallet (wallet 2) endpoints
                            h3 <- activateContractWallet (wallet 3) endpoints -- one referral
                            h4 <- activateContractWallet (wallet 4) endpoints
                            h5 <- activateContractWallet (wallet 5) endpoints
              
                            void $ Emulator.waitNSlots 1
                            callEndpoint @"mintAlone" h3 $ MintParams
                                                {
                                                    treasury = (pkh 1), 
                                                    referral= (pkh 1),
                                                    referralTx = [],                                                                                                                      
                                                    mpAdaAmount = 200_000_000
                                                    
                                                }
                            void $ Emulator.waitNSlots 10
                            callEndpoint @"mintWithFriend" h4 $ MintParams -- 
                                          {     
                                            treasury = (pkh 1) , 
                                            referral=  (pkh 2),  
                                            referralTx = [],         
                                            mpAdaAmount = 400_000_000
                                          }
                            void $ Emulator.waitNSlots 10
                            callEndpoint @"mintWithFriend" h5 $ MintParams -- 
                                         {     
                                             treasury = (pkh 1) , 
                                             referral=  (pkh 2),  
                                             referralTx = [],         
                                             mpAdaAmount = 200_000_000
                                         }
                                                                                                                                                                                                                                                                                                                                                                                                                                                           
                           
                           
                           
                           
                           
                           
                           
                           
























