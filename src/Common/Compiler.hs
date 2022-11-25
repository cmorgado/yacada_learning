{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores  #-}

module Common.Compiler (writeRedeemer, mpa) where

import           Cardano.Api
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Value
import           PlutusTx.Prelude           hiding (Semigroup (..), unless)
import           Prelude                    (IO)
import           Common.Utils
import           Common.TypesV1           
import           Wallet.Emulator.Wallet 
import           Ledger                     hiding (mint, singleton)
import           Ledger.Ada                 as Ada
import           Common.UtilsV1             as U




wallet :: Integer -> Wallet
wallet = knownWallet

pkh :: Integer -> PaymentPubKeyHash
pkh x = mockWalletPaymentPubKeyHash $ wallet x

mpa :: MintParams
mpa =  MintParams
    {
        treasury = pkh 1,
        referral = pkh 1,
        referralTx = [          

        ],
        mpAdaAmount = 200000000     
    }

writeRedeemer  :: MintParams ->  IO ()
writeRedeemer mp = writeJSON "output/redeemer.json" mp




