{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Configuration.RiskInputs {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  RiskInputs(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)


data RiskInputs =
  RiskInputs
    {
      adoptionRateMultiplier          :: Double --  1
    , fuelEconomyMultiplier           :: Double --  1
    , fuelEconomyParity               :: Double --  1  -- FIXME: this should be a time series dependent on the ratio of FCEV to ICE fuel economies
    , gasolinePriceModifier           :: String --  AEO_LP, AEO_REF, AEO_HP
    , energyPricesModifier            :: String --  AEO_LP, AEO_REF, AEO_HP
    , hydrogenCostMultiplier          :: Double --  1
    , stationCapitalCostMultiplier    :: Double --  1
    , debtInterestRateMultiplier      :: Double --  1
    , maximumReturnOnEquityMultiplier :: Double --  1
    }
    deriving (Generic, Read, Show)

instance FromJSON RiskInputs

instance ToJSON RiskInputs where
  toJSON = genericToJSON defaultOptions


instance Tabulatable RiskInputs where
  labels _ =
    [
      "RISK INPUTS"
    , "Adoption Rate Multiplier [fraction]"
    , "Fuel Economy Multiplier [fraction]"
    , "Fuel Economy Parity [fraction]"
    , "Gasoline Price Modifier"
    , "Energy Prices Modifier"
    , "Hydrogen Cost Multiplier"
    , "Station Capital Cost Muliplier [fraction]"
    , "Debt Interest Rate Muliplier [fraction]"
    , "Maximum Return on Equity Muliplier [fraction]"
    ]
  tabulation RiskInputs{..} =
    [
      ""
    , show adoptionRateMultiplier
    , show fuelEconomyMultiplier
    , show fuelEconomyParity
    , gasolinePriceModifier
    , energyPricesModifier
    , show hydrogenCostMultiplier
    , show stationCapitalCostMultiplier
    , show debtInterestRateMultiplier
    , show maximumReturnOnEquityMultiplier
    ]
  untabulation = undefined
