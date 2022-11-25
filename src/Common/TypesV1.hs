{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Common.TypesV1 where


import           Ledger
import           Ledger.Value       (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Data.Aeson         (ToJSON, FromJSON)
import qualified PlutusTx
import           PlutusTx.Prelude
import           GHC.Generics       (Generic)

data MintParams  = MintParams
    {  
        treasury :: PaymentPubKeyHash,
        referral :: PaymentPubKeyHash,
        referralTx :: [TxOut],
        mpAdaAmount :: Integer     
    }deriving (Generic, ToJSON, FromJSON) 
PlutusTx.unstableMakeIsData ''MintParams         