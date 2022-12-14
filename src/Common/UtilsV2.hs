{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Common.UtilsV2 (
    calculateYacada,
    giveReferralNFTName,    
    fromLevelGetValueForReferral,
    yacadaName,
    treasuryAda,
    referralAda,
    toTokenName,
    referralLevel,
    upgradeReferralNFTName,
    info,
    hasUTxO,
    mintFlattened,
    valuePaidToAddress,
    hashMinted,
    mintedQtOfValues,
    mintedQtOfValue,
    mintedTokenNames

 ) where
import              Prelude                         (IO, Show (..), String, Semigroup (..),read, Double)
import              PlutusTx.Prelude                hiding (Semigroup(..), unless)
import              PlutusTx.Builtins               (divideInteger, multiplyInteger)
import              Plutus.V1.Ledger.Value
import              Plutus.V1.Ledger.Time           (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import              Plutus.V1.Ledger.Bytes          (getLedgerBytes)        
import qualified    Plutus.V2.Ledger.Api            as PlutusV2
import qualified    Plutus.V2.Ledger.Contexts       as PlutusV2
import              Data.Hex
import              Data.String                     (IsString (..))




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

{-# INLINABLE referralLevel #-}
referralLevel :: TokenName -> Integer
referralLevel tn = read (take 2 (toString tn))

{-# INLINABLE fromLevelGetValueForReferral #-} -- extract from the Referral NFT the current base level
fromLevelGetValueForReferral :: String -> Integer
fromLevelGetValueForReferral nftName =  read (take 2 nftName)

{-# INLINABLE giveReferralNFTName #-}
giveReferralNFTName :: Integer -> POSIXTime -> TokenName 
giveReferralNFTName ada time  
        | ada == 200_000_000     =  toTokenName ("05_YACADA_REFERRAL_" ++ show(getPOSIXTime time)) -- getPOSIXTime
        | ada == 400_000_000     =  toTokenName ("10_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        | ada == 600_000_000     =  toTokenName ("15_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        | ada == 800_000_000     =  toTokenName ("20_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        | ada == 1_000_000_000   =  toTokenName ("25_YACADA_REFERRAL_" ++  show(getPOSIXTime time))
        | otherwise              =  toTokenName ("00")

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
info :: PlutusV2.ScriptContext -> PlutusV2.TxInfo
info = PlutusV2.scriptContextTxInfo

{-# INLINABLE hasUTxO #-}
hasUTxO :: PlutusV2.TxOutRef -> PlutusV2.ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> PlutusV2.txInInfoOutRef i == utxo) $ PlutusV2.txInfoInputs (info ctx)  

{-# INLINEABLE mintFlattened #-}
mintFlattened :: PlutusV2.ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ PlutusV2.txInfoMint (info ctx)

{-# INLINEABLE mintedValues #-}
mintedValues :: PlutusV2.ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintedValues ctx = filter (\(cs,_,_) -> cs  == PlutusV2.ownCurrencySymbol ctx) $ mintFlattened ctx

{-# INLINEABLE mintedQtOfValues #-}
mintedQtOfValues :: PlutusV2.ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintedQtOfValues ctx = filter (\(cs,_,_) -> cs  == PlutusV2.ownCurrencySymbol ctx) $ mintFlattened ctx

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
isAddrGettingPaid :: [PlutusV2.TxOut] -> PlutusV2.Address -> Value -> Bool
isAddrGettingPaid []     _    _ = False
isAddrGettingPaid (x:xs) addr val
  | checkAddr && checkVal = True -- found utxo with reward payout
  | otherwise             = isAddrGettingPaid xs addr val
  where
    checkAddr :: Bool
    checkAddr = PlutusV2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = PlutusV2.txOutValue x == val -- exact reward only


{-# INLINEABLE valuePaidToAddress #-}
valuePaidToAddress :: PlutusV2.ScriptContext -> PlutusV2.Address -> Value
valuePaidToAddress ctx addr = mconcat
  (fmap PlutusV2.txOutValue (filter (\x -> PlutusV2.txOutAddress x == addr)
  (PlutusV2.txInfoOutputs (info ctx))))    

{-# INLINEABLE fst' #-}
fst' :: (a,b,c) -> a
fst' (x,_,_) = x 

{-# INLINEABLE snd' #-}
snd' :: (a,b,c) -> b
snd' (_,x,_) = x 

{-# INLINEABLE trd' #-}
trd' :: (a,b,c) -> c
trd' (_,_,x) = x 