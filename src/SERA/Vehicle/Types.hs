{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}


module SERA.Vehicle.Types (
  Age
, FAge
, fAge
, ModelYear
, FModelYear
, fModelYear
, Classification(..)
, FClassification
, fClassification
, Fuel(..)
, FFuel
, fFuel
, MarketShare
, FMarketShare
, fMarketShare
, Sales
, FSales
, fSales
, Stock
, FStock
, fStock
, FuelSplit
, FFuelSplit
, fFuelSplit
, Survival
, FSurvival
, fSurvival
, AnnualTravel
, FAnnualTravel
, fAnnualTravel
, FuelEconomy
, FFuelEconomy
, fFuelEconomy
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import GHC.Generics (Generic)
import SERA.Types (quotedStringTypes)


-- | A vehicle age.
type Age = Int


type FAge = '("Age [yr]", Age)


fAge :: SField FAge
fAge = SField


type ModelYear = Int


type FModelYear = '("Model Year [yr]", ModelYear)


fModelYear :: SField FModelYear
fModelYear = SField


newtype Classification = Classification {classification :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Classification where
  readsPrec
    | quotedStringTypes = (fmap (first Classification) .) . readsPrec
    | otherwise         = const $ return . (, []) . Classification

instance Show Classification where
  show
    | quotedStringTypes = show . classification
    | otherwise         = classification

instance FromJSON Classification where
  parseJSON = withText "SERA.Vehicle.Types.Classification" $ return . Classification . toString

instance ToJSON Classification where
  toJSON = toJSON . classification


type FClassification = '("Classification", Classification)


fClassification :: SField FClassification
fClassification = SField


newtype Fuel = Fuel {fuel :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Fuel where
  readsPrec
    | quotedStringTypes = (fmap (first Fuel) .) . readsPrec
    | otherwise         = const $ return . (, []) . Fuel

instance Show Fuel where
  show = show . fuel

instance FromJSON Fuel where
  parseJSON = withText "SERA.Vehicle.Types.Fuel" $ return . Fuel . toString

instance ToJSON Fuel where
  toJSON = toJSON . fuel


type FFuel = '("Fuel Type", Fuel)


fFuel :: SField FFuel
fFuel = SField


type Sales = Double


type FSales = '("Sales [veh]", Sales)


fSales :: SField FSales
fSales = SField


type Stock = Double


type FStock = '("Stock [veh]", Stock)


fStock :: SField FStock
fStock = SField


type MarketShare = Double


type FMarketShare = '("Market Share [1]", MarketShare)


fMarketShare :: SField FMarketShare
fMarketShare = SField


type FuelSplit = Double


type FFuelSplit = '("Fraction Travel [1]", FuelSplit)


fFuelSplit :: SField FFuelSplit
fFuelSplit = SField


type Survival = Double


type FSurvival = '("Annual Survival [1/yr]", Survival)


fSurvival :: SField FSurvival
fSurvival = SField


type AnnualTravel = Double


type FAnnualTravel = '("Travel [mi/yr]", AnnualTravel)


fAnnualTravel :: SField FAnnualTravel
fAnnualTravel = SField


type FuelEconomy = Double


type FFuelEconomy = '("Fuel Economy [mi/gge]", FuelEconomy)


fFuelEconomy :: SField FFuelEconomy
fFuelEconomy = SField
