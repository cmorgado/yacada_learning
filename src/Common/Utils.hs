{-# LANGUAGE NumericUnderscores  #-}


module Common.Utils (
    calculateYacada,
    giveReferralNFTName,
    validateYacadaMint,
    fromLevelGetValueForReferral,
    yacadaName,
    treasuryAda,
    referralAda,
    toTokenName,
    referralLevel,
    info,
    hasUTxO,
    mintFlattened,
    valuePaidToAddress

 ) where

import              Prelude                (IO, Show (..), String, Semigroup (..),read, Double)
import              Ledger
import              PlutusTx.Prelude       hiding (Semigroup(..), unless)
import              PlutusTx.Builtins      (divideInteger, multiplyInteger)
import              Ledger.Value           as Value
import              Plutus.V1.Ledger.Value
import              Plutus.V1.Ledger.Time  (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import              Plutus.V1.Ledger.Bytes (getLedgerBytes)
import              Data.Hex
import              Data.String            (IsString (..))
        

{-# INLINABLE calculateYacada #-} -- calculates the yacada minted from the amount of Ada sent (amount sent is a fix set of values) -- can inprove via custom data?
calculateYacada :: Integer -> Integer
calculateYacada ada = case ada of 
    200_000_000     ->  1000
    400_000_000     ->  2040 -- minting bonus 10%
    600_000_000     ->  3090 -- minting bonus 15%
    800_000_000     ->  4160 -- minting bonus 20%
    1000_000_000    ->  5250 -- minting bonus 15%
    _       ->  0
       
{-# INLINABLE treasuryAda #-} -- this calculates the ADA that goes to the treasury wallet based on referral level (or no referral)
treasuryAda :: Integer -> Integer -> Integer
treasuryAda ada referral = ( (ada*10000) * (10000 - (referral*100) ) ) `divideInteger` 100000000
    
{-# INLINABLE referralAda #-} -- this calculates the ADA sent to the referral wallet
referralAda :: Integer -> Integer -> Integer
referralAda ada referral = ada - (treasuryAda ada referral)

{-# INLINABLE referralLevel #-}
referralLevel :: TokenName -> Integer
referralLevel tn = read (take 2 (toString tn))

{-# INLINABLE fromLevelGetValueForReferral #-} -- extract from the Referral NFT the current base level
fromLevelGetValueForReferral :: String -> Integer
fromLevelGetValueForReferral nftName =  read (take 2 nftName)

{-# INLINABLE giveReferralNFTName #-}
giveReferralNFTName :: Integer -> POSIXTime -> TokenName 
giveReferralNFTName ada time = case ada of 
        200_000_000     ->  toTokenName ("05_YACADA_REFERRAL_"++ show(getPOSIXTime time)) -- getPOSIXTime
        400_000_000     ->  toTokenName ("10_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        600_000_000     ->  toTokenName ("15_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        800_000_000     ->  toTokenName ("20_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        1_000_000_000    ->  toTokenName ("25_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        _       ->  toTokenName ("00")

{-# INLINABLE yacadaName #-}
yacadaName :: TokenName
yacadaName = toTokenName "YACADA_TOKEN"

{-# INLINABLE toTokenName #-}
toTokenName :: String -> TokenName
toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn }

{-# INLINABLE validateYacadaMint #-}
validateYacadaMint :: CurrencySymbol -> ScriptContext -> Bool
validateYacadaMint _ _ = True
-- validateYacadaMint csm ctx = case mintFlattened ctx of
--      [(cs, tn, amt)] -> (cs == csm) &&
--                         (tn == yacadaName) &&
--                         (amt == 200)
--      _               -> False


{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)  

{-# INLINEABLE mintFlattened #-}
mintFlattened :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ txInfoMint (info ctx)
    
{-# INLINEABLE valuePaidToAddress #-}
valuePaidToAddress :: ScriptContext -> Address -> Value
valuePaidToAddress ctx addr = mconcat
  (fmap txOutValue (filter (\x -> txOutAddress x == addr)
  (txInfoOutputs (info ctx))))    