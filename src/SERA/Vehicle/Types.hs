{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}


module SERA.Vehicle.Types (
-- * Age
  Age
, FAge
, fAge
-- * Model year
, ModelYear
, FModelYear
, fModelYear
-- * Vehicle classification
, Classification(..)
, FClassification
, fClassification
-- * Fuel type
, Fuel(..)
, FFuel
, fFuel
-- * Market share
, MarketShare
, FMarketShare
, fMarketShare
-- * Emission type
, Emission
, FEmission
, fEmission
-- * Vehicle sales
, Sales
, FSales
, fSales
-- * Vehicle stock
, Stock
, FStock
, fStock
-- * Fuel split
, FuelSplit
, FFuelSplit
, fFuelSplit
-- * Vehicle survival
, Survival
, FSurvival
, fSurvival
-- * Vehicle travel
, AnnualTravel
, FAnnualTravel
, fAnnualTravel
-- * Fuel efficiency
, FuelEfficiency
, FFuelEfficiency
, fFuelEfficiency
-- * Fractional travel
, FractionTravel
, FFractionTravel
, fFractionTravel
-- * Emission factor
, EmissionFactor
, FEmissionFactor
, fEmissionFactor
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


type FModelYear = '("Model Year", ModelYear)


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
  show
    | quotedStringTypes = show . fuel
    | otherwise         = fuel

instance FromJSON Fuel where
  parseJSON = withText "SERA.Vehicle.Types.Fuel" $ return . Fuel . toString

instance ToJSON Fuel where
  toJSON = toJSON . fuel


type FFuel = '("Fuel Type", Fuel)


fFuel :: SField FFuel
fFuel = SField


newtype Emission = Emission {emission :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Emission where
  readsPrec
    | quotedStringTypes = (fmap (first Emission) .) . readsPrec
    | otherwise         = const $ return . (, []) . Emission

instance Show Emission where
  show
    | quotedStringTypes = show . emission
    | otherwise         = emission

instance FromJSON Emission where
  parseJSON = withText "SERA.Vehicle.Types.Emission" $ return . Emission . toString

instance ToJSON Emission where
  toJSON = toJSON . emission


type FEmission = '("Emission", Emission)

fEmission :: SField Emission
fEmission = SField


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


type FuelEfficiency = Double


type FFuelEfficiency = '("Fuel Efficiency [mi/gge]", FuelEfficiency)


fFuelEfficiency :: SField FFuelEfficiency
fFuelEfficiency = SField


type FractionTravel = Double


type FFractionTravel = '("Fraction Travel [mi/mi]", FractionTravel)


fFractionTravel :: SField FFractionTravel
fFractionTravel = SField


type EmissionFactor = Double


type FEmissionFactor = '("Emission Factor [g/gge]", EmissionFactor)


fEmissionFactor :: SField FEmissionFactor
fEmissionFactor = SField
