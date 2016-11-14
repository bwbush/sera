{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Risks {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  applyRisks
, applyRisks'
) where


import AEO.Prices (lookupAEO)
import AEO.Types (Region(..), Sector(Commercial, Transportation), Source(Electricity, MotorGasoline, NaturalGas))
import Control.Monad (ap)
import SERA.Configuration.RiskInputs (RiskInputs(..))
import SERA.Configuration.ScenarioInputs (ScenarioInputs(..))
import SERA.Finance.Capital (Capital(..))
import SERA.Finance.Scenario (Scenario(..))
import SERA.Util.Units (CurrencyUnits(Dollars), EnergyUnits(Gge, Kwh, MillionBtu))


class Risky a where
  applyRisks :: RiskInputs -> a -> a
  applyRisks' :: Maybe RiskInputs -> a -> a
  applyRisks' = flip (ap maybe (flip applyRisks))


instance Risky ScenarioInputs where
  applyRisks RiskInputs{..} scenarioInputs@ScenarioInputs{..} =
    scenarioInputs
      {
        debtInterestRate      = debtInterestRate      * debtInterestRateMultiplier
      }


instance Risky Capital where
  applyRisks RiskInputs{..} stationCharacteristics@Station{..} =
    stationCharacteristics
      {
        stationCapitalCost = stationCapitalCost * stationCapitalCostMultiplier
      }


instance Risky Scenario where
  applyRisks RiskInputs{..} scenario@Scenario{..} =
    scenario
      {
        hydrogenPrice        = fuelEconomyParity * lookupAEO Dollars Gge gasolinePriceModifier UnitedStates Transportation MotorGasoline scenarioYear'
      , electricityCost      = lookupAEO Dollars Kwh energyPricesModifier UnitedStates Commercial Electricity scenarioYear'
      , naturalGasCost       = lookupAEO Dollars MillionBtu energyPricesModifier UnitedStates Commercial NaturalGas scenarioYear'
      , hydrogenCost         = hydrogenCost * hydrogenCostMultiplier
      , stationUtilization   = stationUtilization / adoptionRateMultiplier * fuelEconomyMultiplier
      , hydrogenSales        = hydrogenSales * adoptionRateMultiplier / fuelEconomyMultiplier
      , fcevTotal            = round $ fromIntegral fcevTotal * adoptionRateMultiplier
      , fcevNew              = round $ fromIntegral fcevNew * adoptionRateMultiplier
      , economyNet           = economyNet * fuelEconomyMultiplier
      , economyNew           = economyNew * fuelEconomyMultiplier
      }
    where
      scenarioYear' = minimum [2040, scenarioYear]
