{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Common.UtilsV1 (
    calculateYacada,
    giveReferralNFTName,       
    yacadaName,
    treasuryAda,
    referralAda,
    toTokenName,
    upgradeReferralNFTName,
    info,
    hasUTxO,
    mintFlattened,
    valuePaidToAddress,
    hashMinted,
    mintedQtOfValues,
    mintedQtOfValue,
    mintedTokenNames,
    giveReferralNFTValue
  


 

 ) where
import              Data.Aeson             (ToJSON, FromJSON)
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
import              GHC.Generics           (Generic)

{-# INLINABLE giveReferralNFTValue #-}
giveReferralNFTValue :: Integer -> Integer
giveReferralNFTValue ada   
        | ada == 200_000_000     =  5
        | ada == 400_000_000     =  10
        | ada == 600_000_000     =  15
        | ada == 800_000_000     =  20
        | ada == 1_000_000_000   =  25
        | otherwise              =  0

{-# INLINABLE calculateYacada #-} -- calculates the yacada minted from the amount of Ada sent (amount sent is a fix set of values) -- can inprove via custom data?
calculateYacada :: Integer -> Integer
calculateYacada ada = case ada of 
    200_000_000     ->  1000
    400_000_000     ->  2040 -- minting bonus 10%
    600_000_000     ->  3090 -- minting bonus 15%
    800_000_000     ->  4160 -- minting bonus 20%
    1000_000_000    ->  5250 -- minting bonus 15%
    _               ->  0
       
{-# INLINABLE treasuryAda #-} -- this calculates the ADA that goes to the treasury wallet based on referral level (or no referral)
treasuryAda :: Integer -> Integer -> Integer
treasuryAda ada referral = ( (ada*10000) * (10000 - (referral*100) ) ) `divideInteger` 100000000
    
{-# INLINABLE referralAda #-} -- this calculates the ADA sent to the referral wallet
referralAda :: Integer -> Integer -> Integer
referralAda ada referral = ada - (treasuryAda ada referral)


{-# INLINABLE giveReferralNFTName #-}
giveReferralNFTName :: TokenName 
giveReferralNFTName =  toTokenName "YACADA_REFERRAL"
     
     
     
     
     
     

{-# INLINABLE upgradeReferralNFTName #-}
upgradeReferralNFTName :: Integer -> POSIXTime -> TokenName
upgradeReferralNFTName up time = do
    if (up<50)
        then toTokenName ("01_YACADA_REFERRAL_"++ show(getPOSIXTime time)) --
    else
        toTokenName ("00_YACADA_REFERRAL_"++ show(getPOSIXTime time))  
      
      
      

{-# INLINABLE yacadaName #-}
yacadaName :: TokenName
yacadaName = toTokenName "YACADA_TOKEN"

{-# INLINABLE toTokenName #-}
toTokenName :: String -> TokenName
toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn }

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)  

{-# INLINEABLE mintFlattened #-}
mintFlattened :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ txInfoMint (info ctx)

{-# INLINEABLE mintedValues #-}
mintedValues :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintedValues ctx = filter (\(cs,_,_) -> cs  == ownCurrencySymbol ctx) $ mintFlattened ctx

{-# INLINEABLE mintedQtOfValues #-}
mintedQtOfValues :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintedQtOfValues ctx = filter (\(cs,_,_) -> cs  == ownCurrencySymbol ctx) $ mintFlattened ctx

{-# INLINEABLE mintedTokenNames #-}
mintedTokenNames :: [(CurrencySymbol, TokenName, Integer)] -> [String] -> [String]
mintedTokenNames [] tns = tns
mintedTokenNames (x:xs) tns = mintedTokenNames xs ( tns ++ [toString (snd' x)] )

{-# INLINEABLE mintedQtOfValue #-}
mintedQtOfValue :: CurrencySymbol -> [(CurrencySymbol, TokenName, Integer)] -> Integer -> Integer
mintedQtOfValue cs [] v = v
mintedQtOfValue cs (x:xs) i = do
    let currSym = fst' x 
    let tokName = snd' x
    let qt = trd' x  
    if currSym == cs        
        then            
            mintedQtOfValue cs xs $ qt + i
    else
        mintedQtOfValue cs xs i
  

{-# INLINEABLE hashMinted #-}
hashMinted :: CurrencySymbol ->  [(CurrencySymbol, TokenName, Integer)] -> Bool
hashMinted csi minted  = any (\(cs,_,_) -> cs == csi) minted

{-# INLINEABLE isAddrGettingPaid #-}
isAddrGettingPaid :: [TxOut] -> Address -> Value -> Bool
isAddrGettingPaid []     _    _ = False
isAddrGettingPaid (x:xs) addr val
  | checkAddr && checkVal = True -- found utxo with reward payout
  | otherwise             = isAddrGettingPaid xs addr val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = txOutValue x == val -- exact reward only


{-# INLINEABLE valuePaidToAddress #-}
valuePaidToAddress :: ScriptContext -> Address -> Value
valuePaidToAddress ctx addr = mconcat
  (fmap txOutValue (filter (\x -> txOutAddress x == addr)
  (txInfoOutputs (info ctx))))    

{-# INLINEABLE fst' #-}
fst' :: (a,b,c) -> a
fst' (x,_,_) = x 

{-# INLINEABLE snd' #-}
snd' :: (a,b,c) -> b
snd' (_,x,_) = x 

{-# INLINEABLE trd' #-}
trd' :: (a,b,c) -> c
trd' (_,_,x) = x 