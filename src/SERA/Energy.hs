{-# LANGUAGE DeriveGeneric #-}


module SERA.Energy {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  EnergyCosts(..)
, costOfElectricity'
, costOfNaturalGas'
, costOfDeliveredH2ToStation'
, pumpPriceOfHydrogen'
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import GHC.Generics (Generic)
import Math.GeometricSeries (GeometricSeries, asFunction)
import SERA.Finance.Capital ()


data EnergyCosts =
  EnergyCosts
    {
      costOfElectricity          :: GeometricSeries Double
    , costOfNaturalGas           :: GeometricSeries Double
    , costOfDeliveredH2ToStation :: GeometricSeries Double
    , pumpPriceOfHydrogen        :: GeometricSeries Double
    }
    deriving (Generic, Read, Show)

instance FromJSON EnergyCosts

instance ToJSON EnergyCosts where
  toJSON = genericToJSON defaultOptions


costOfElectricity' :: EnergyCosts -> Int -> Double
costOfElectricity' = (. fromIntegral) . asFunction . costOfElectricity


costOfNaturalGas' :: EnergyCosts -> Int -> Double
costOfNaturalGas' = (. fromIntegral) . asFunction . costOfNaturalGas


costOfDeliveredH2ToStation' :: EnergyCosts -> Int -> Double
costOfDeliveredH2ToStation' = (. fromIntegral) . asFunction . costOfDeliveredH2ToStation


pumpPriceOfHydrogen' :: EnergyCosts -> Int -> Double
pumpPriceOfHydrogen' = (. fromIntegral) . asFunction .pumpPriceOfHydrogen
