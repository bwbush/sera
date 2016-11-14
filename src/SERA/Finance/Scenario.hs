{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Scenario {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Scenario(..)
, zeroScenario
, zeroScenarios
, resetEnergyCosts
, economyNetTable
, economyNewTable
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..), nan)
import Data.Maybe (fromMaybe)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Energy (EnergyCosts(..), costOfElectricity', costOfNaturalGas', costOfDeliveredH2ToStation', pumpPriceOfHydrogen')
import SERA.Util.Summarization (Summable(..))


data Scenario =
  Scenario
    {
      -- TODO: put inflation rate and energy prices into scenario
      scenarioYear               :: Int
    , durationOfDebt             :: Int
    , newCapitalIncentives       :: Double
    , newProductionIncentives    :: Double
    , newFuelPrepayments         :: Double
    , newCrowdFunding            :: Double
    , newConsumerDiscounts       :: Double
    , newCapitalExpenditures     :: Double
    , electricityCost            :: Double       --  $/kWh
    , naturalGasCost             :: Double       --  $/mmBTU
    , hydrogenCost               :: Double       --  $/kgH2
    , hydrogenPrice              :: Double       --  $/kgH2
    , stationUtilization         :: Double       --  %
    , hydrogenSales              :: Double
    , fcevTotal                  :: Int
    , fcevNew                    :: Int
    , economyNet                 :: Double
    , economyNew                 :: Double
    , vmtTotal                   :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON Scenario

instance ToJSON Scenario where
  toJSON = genericToJSON defaultOptions

instance Summable Scenario where
  summation x =
    Scenario
    {
      scenarioYear            = head  $ map scenarioYear             x  
    , durationOfDebt          = head  $ map durationOfDebt           x
    , newCapitalIncentives    = sum   $ map newCapitalIncentives     x
    , newProductionIncentives = sum   $ map newProductionIncentives  x 
    , newFuelPrepayments      = sum   $ map newFuelPrepayments       x
    , newCrowdFunding         = sum   $ map newCrowdFunding          x
    , newConsumerDiscounts    = sum   $ map newConsumerDiscounts     x
    , newCapitalExpenditures  = sum   $ map newCapitalExpenditures   x
    , electricityCost         = sum'  $ map electricityCost          x 
    , naturalGasCost          = sum'  $ map naturalGasCost           x 
    , hydrogenCost            = sum'  $ map hydrogenCost             x 
    , hydrogenPrice           = sum'  $ map hydrogenPrice            x  
    , stationUtilization      = sum'' $ map stationUtilization       x
    , hydrogenSales           = sum   $ map hydrogenSales            x  
    , fcevTotal               = sum   $ map fcevTotal                x  
    , fcevNew                 = sum   $ map fcevNew                  x
    , economyNet              = nan  
    , economyNew              = nan  
    , vmtTotal                = nan  
    }
      where
        weight = map hydrogenSales x
        sum' y = sum (zipWith (*) y weight) / sum weight
        sum'' y = sum weight / sum (zipWith (\u v -> if u == 0 then 0 else u / v) weight y)

instance Zero Scenario where
  zero = Scenario 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 nan nan nan

instance Tabulatable Scenario where
  labels _ =
    [
      "SCENARIO"
    , "Year"
    , "Duration of Debt [year]"
    , "New Capital Incentives [$]"
    , "New Production Incentives [$]"
    , "New Fuel Prepayments [$]"
    , "New Crowd-Funding [$]"
    , "New Consumer Discounts [$]"
    , "New Capital Expenditure [$]"
    , "Electricity Cost [$ / kW h]"
    , "Natural Gas Cost [$ / mmBTU]"
    , "Hydrogen Cost [$ / kgH2]"
    , "Hydrogen Price [$ / kgH2]"
    , "Station Utilization [fraction]"
    , "Hydrogen Sales [kgH2 / day]"
    , "Total FCEVs [vehicle]"
    , "New FCEVs [vehicle]"
    , "Net FCEV Fuel Economy [mi / gge]"
    , "New FCEV Fuel Economy [mi / gge]"
    , "VMT [mi / vehicle]"
    ]
  tabulation Scenario{..} =
    [
      ""
    , show scenarioYear
    , show durationOfDebt
    , show newCapitalIncentives
    , show newProductionIncentives
    , show newFuelPrepayments
    , show newCrowdFunding
    , show newConsumerDiscounts
    , show newCapitalExpenditures
    , show electricityCost
    , show naturalGasCost
    , show hydrogenCost
    , show hydrogenPrice
    , show stationUtilization
    , show hydrogenSales
    , show fcevTotal
    , show fcevNew
    , show economyNet
    , show economyNew
    , show vmtTotal
    ]
  untabulation = undefined


resetEnergyCosts :: EnergyCosts -> Scenario -> Scenario
resetEnergyCosts energyCosts scenario@Scenario{..} =
  scenario
  {
    electricityCost = costOfElectricity' energyCosts scenarioYear
  , naturalGasCost  = costOfNaturalGas' energyCosts scenarioYear
  , hydrogenCost    = costOfDeliveredH2ToStation' energyCosts scenarioYear
  , hydrogenPrice   = pumpPriceOfHydrogen' energyCosts scenarioYear
  }


zeroScenario :: EnergyCosts -> Int -> Scenario
zeroScenario energyCosts year =
  resetEnergyCosts energyCosts
  Scenario {
    scenarioYear               = year
  , durationOfDebt             = 0
  , newCapitalIncentives       = 0
  , newProductionIncentives    = 0
  , newFuelPrepayments         = 0
  , newCrowdFunding            = 0
  , newConsumerDiscounts       = 0
  , newCapitalExpenditures     = 0
  , electricityCost            = nan
  , naturalGasCost             = nan
  , hydrogenCost               = nan
  , hydrogenPrice              = nan
  , stationUtilization         = nan
  , hydrogenSales              = nan
  , fcevTotal                  = 0
  , fcevNew                    = 0
  -- FIXME: compute these dynamically
  , economyNet                 = fromMaybe nan $ lookup year economyNetTable
  , economyNew                 = fromMaybe nan $ lookup year economyNewTable
  , vmtTotal                   = nan
  }


zeroScenarios :: EnergyCosts -> Int -> Scenario -> [Scenario]
zeroScenarios energyCosts lastYear scenario = scenario : map (zeroScenario energyCosts) [(scenarioYear scenario + 1)..lastYear]


economyNewTable :: [(Int, Double)]
economyNewTable =
  [
    (2011, 68.59)
  , (2012, 68.59)
  , (2013, 68.59)
  , (2014, 68.59)
  , (2015, 68.59)
  , (2016, 68.19)
  , (2017, 68.19)
  , (2018, 67.98)
  , (2019, 67.77)
  , (2020, 67.56)
  , (2021, 67.43)
  , (2022, 67.3)
  , (2023, 67.11)
  , (2024, 67.86)
  , (2025, 68.39)
  , (2026, 68.73)
  , (2027, 68.97)
  , (2028, 69.21)
  , (2029, 69.22)
  , (2030, 69.07)
  , (2031, 69.1)
  , (2032, 69.19)
  , (2033, 69.34)
  , (2034, 69.55)
  , (2035, 69.78)
  , (2036, 70.03)
  , (2037, 70.27)
  , (2038, 70.48)
  , (2039, 70.68)
  , (2040, 70.85)
  , (2041, 70.99)
  , (2042, 71.11)
  , (2043, 71.26)
  , (2044, 71.37)
  , (2045, 71.47)
  , (2046, 71.59)
  , (2047, 71.7)
  , (2048, 71.81)
  , (2049, 71.92)
  , (2050, 72.01)
  ]


economyNetTable :: [(Int, Double)]
economyNetTable =
  [
    (2011, 68.59)
  , (2012, 68.59)
  , (2013, 68.59)
  , (2014, 68.59)
  , (2015, 68.59)
  , (2016, 68.33585774419166)
  , (2017, 68.25313820327338)
  , (2018, 68.17907465753257)
  , (2019, 68.06875550217693)
  , (2020, 67.93252384297088)
  , (2021, 67.79854766822555)
  , (2022, 67.66643615524015)
  , (2023, 67.51805089004185)
  , (2024, 67.6021313539701)
  , (2025, 67.80178195805063)
  , (2026, 68.03863039994752)
  , (2027, 68.27724787151068)
  , (2028, 68.51630514484093)
  , (2029, 68.69826856493891)
  , (2030, 68.79789658077945)
  , (2031, 68.87873876338467)
  , (2032, 68.95925474588289)
  , (2033, 69.05240022415526)
  , (2034, 69.16689091304964)
  , (2035, 69.29985037220116)
  , (2036, 69.44878666985294)
  , (2037, 69.60701611555957)
  , (2038, 69.76694874005594)
  , (2039, 69.92653792010802)
  , (2040, 70.08179886004847)
  , (2041, 70.23022362974294)
  , (2042, 70.37134245339506)
  , (2043, 70.51108396130307)
  , (2044, 70.64517296879404)
  , (2045, 70.77348570727352)
  , (2046, 70.89925617841995)
  , (2047, 71.02166117242146)
  , (2048, 71.14100384959282)
  , (2049, 71.25753543509616)
  , (2050, 71.36953304469198)
  ]
