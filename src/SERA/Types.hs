{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TupleSections              #-}


module SERA.Types (
  Region(..)
, FRegion
, fRegion
, Year
, FYear
, fYear
, quotedStringTypes
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import GHC.Generics (Generic)


quotedStringTypes :: Bool
quotedStringTypes = False


newtype Region = Region {region :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Region where
  readsPrec
    | quotedStringTypes = (fmap (first Region) .) . readsPrec
    | otherwise         = const $ return . (, []) . Region

instance Show Region where
  show
    | quotedStringTypes = show . region
    | otherwise         = region

instance FromJSON Region where
  parseJSON = withText "SERA.Types.Region" $ return . Region . toString

instance ToJSON Region where
  toJSON = toJSON . region


type FRegion = '("Region", Region)


fRegion :: SField FRegion
fRegion = SField


type Year = Int


type FYear = '("Year [yr]", Year)


fYear :: SField FYear
fYear = SField
