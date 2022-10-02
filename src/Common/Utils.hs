module Common.Utils (
    calculateYacada,
    giveReferralNFTName,
    yacadaName,
    toTokenName

 ) where

import              Prelude                (IO, Show (..), String, Semigroup (..))
import              Ledger
import              PlutusTx.Prelude       hiding (Semigroup(..), unless)
import              Ledger.Value           as Value
import              Plutus.V1.Ledger.Value
import              Plutus.V1.Ledger.Time  (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import              Plutus.V1.Ledger.Bytes (getLedgerBytes)
import              Data.Hex
import              Data.String            (IsString (..))

{-# INLINABLE calculateYacada #-}
calculateYacada :: Integer -> Integer
calculateYacada ada = case ada of 
    200     ->  1000
    400     ->  2000
    600     ->  3000
    800     ->  4000
    1000    ->  5000
    _       ->  0


{-# INLINABLE giveReferralNFTName #-}
giveReferralNFTName :: Integer -> POSIXTime -> TokenName 
giveReferralNFTName ada time = case ada of 
        200     ->  toTokenName ( "YACADA_REFERRAL_L01_"++ show(getPOSIXTime time)) -- getPOSIXTime
        400     ->  toTokenName ("YACADA_REFERRAL_L02_" ++  show(getPOSIXTime time))
        600     ->  toTokenName ("YACADA_REFERRAL_L03_" ++  show(getPOSIXTime time))
        800     ->  toTokenName ("YACADA_REFERRAL_L04_" ++  show(getPOSIXTime time))
        1000    ->  toTokenName ("YACADA_REFERRAL_L05_" ++  show(getPOSIXTime time))
        _       ->  toTokenName ("Error")

{-# INLINABLE yacadaName #-}
yacadaName :: TokenName
yacadaName = toTokenName "YACADA_TOKEN"

{-# INLINABLE toTokenName #-}
toTokenName :: String -> TokenName
toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn }