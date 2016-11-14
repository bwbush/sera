{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module SERA.Finance.Capital {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Capital(..)
, costStation
, Operations(..)
, Quantity
, costOperations
, scaleOperations
, ageStationTo
, ageStation
, combineStations
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Maybe (fromJust)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import Math.GeometricSeries (GeometricSeries(..), asFunction)
import SERA.Refueling.FCVSE.Cost.NREL56412 (Quantity, capitalCost, installationCost, maintenanceCost, rentCost)
import SERA.Util.Summarization (Summable(..))
import SERA.Util.Time (Year(..))


type DoubleSeries = GeometricSeries Double

instance FromJSON DoubleSeries

instance ToJSON DoubleSeries where
  toJSON = genericToJSON defaultOptions


data Capital =
  Station
    {
      stationYear                            :: Int     --  year
    , stationTotal                           :: Int     --  1
    , stationOperating                       :: Double  --  1
    , stationNew                             :: Int     --  1
    , stationElectricityUse                  :: Double  --  kW h / kgH2
    , stationNaturalGasUse                   :: Double  --  mmBTU / kgH2
    , stationDeliveredHydrogenUse            :: Double  --  kgH2delivered / kgH2dispensed
    , stationCapacity                        :: Double  --  kg / day
    , stationCapitalCost                     :: Double  --  $
    , stationInstallationCost                :: Double  --  $
    , stationIncidentalRevenue               :: Double  --  $
    , stationMaintenanceCost                 :: Double  --  $ / year
    , stationLicensingAndPermitting          :: Double  --  $ / year
    , stationRentOfLand                      :: Double  --  $ / year
    , stationStaffing                        :: Double  --  hour / year
    , stationLaborRate                       :: Double  --  $ / hour
    , stationSellingAndAdministrativeExpense :: Double  --  %
    , stationCreditCardFeesRate              :: Double  --  %
    , stationSalesTaxRate                    :: Double  --  %
    , stationRoadTax                         :: Double  --  $ / kg
    , stationPropertyTaxRate                 :: Double  --  %
    , stationPropertyInsuranceRate           :: Double  --  %
    }
    deriving (Generic, Read, Show)

instance FromJSON Capital

instance ToJSON Capital where
  toJSON = genericToJSON defaultOptions

instance Zero Capital where
  zero = Station 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

instance Summable Capital where
  summation x =
    Station
    {
      stationYear                            = head $ map stationYear                            x
    , stationTotal                           = sum  $ map stationTotal                           x
    , stationOperating                       = sum  $ map stationOperating                       x
    , stationNew                             = sum  $ map stationNew                             x
    , stationElectricityUse                  = sum' $ map stationElectricityUse                  x
    , stationNaturalGasUse                   = sum' $ map stationNaturalGasUse                   x
    , stationDeliveredHydrogenUse            = sum' $ map stationDeliveredHydrogenUse            x
    , stationCapacity                        = sum  $ map stationCapacity                        x
    , stationCapitalCost                     = sum  $ map stationCapitalCost                     x
    , stationInstallationCost                = sum  $ map stationInstallationCost                x
    , stationIncidentalRevenue               = sum  $ map stationIncidentalRevenue               x
    , stationMaintenanceCost                 = sum  $ map stationMaintenanceCost                 x
    , stationLicensingAndPermitting          = sum  $ map stationLicensingAndPermitting          x
    , stationRentOfLand                      = sum' $ map stationRentOfLand                      x
    , stationStaffing                        = sum  $ map stationStaffing                        x
    , stationLaborRate                       = sum' $ map stationLaborRate                       x
    , stationSellingAndAdministrativeExpense = sum' $ map stationSellingAndAdministrativeExpense x
    , stationCreditCardFeesRate              = sum' $ map stationCreditCardFeesRate              x
    , stationSalesTaxRate                    = sum' $ map stationSalesTaxRate                    x
    , stationRoadTax                         = sum' $ map stationRoadTax                         x
    , stationPropertyTaxRate                 = sum' $ map stationPropertyTaxRate                 x
    , stationPropertyInsuranceRate           = sum' $ map stationPropertyInsuranceRate           x
    }
      where
        weights = map stationCapacity x
        sum' y = sum (zipWith (*) y weights) / sum weights


instance Tabulatable Capital where
  labels _ =
    [
      "STATION CHARACTERISTICS"
    , "Year"
    , "Total Stations [station]"
    , "Operating Stations [station]"
    , "New Stations [station]"
    , "Electricity Use [kW h / kgH2]"
    , "Natural Gas Use [mmBTU / kgH2]"
    , "Delivered Hydrogen Use [kgH2delivered / kgH2dispensed]"
    , "Total Capacity [kg / day]"
    , "Total Capital Cost [$]"
    , "Total Installation Cost [$]"
    , "Total Incidental Revenue [$ / year]"
    , "Total Maintenance Cost [$ / year]"
    , "Total Licensing and Permitting [$ / year]"
    , "Total Rent of Land [$ / year]"
    , "Total Staffing [hour / year]"
    , "Labor Rate [$ / hour]"
    , "Selling and Administrative Expense [fraction]"
    , "Credit Card Fees Rate [fraction]"
    , "Sales Tax Rate [fraction]"
    , "Road Tax [$ / kgH2]"
    , "Property Tax Rate [fraction / year]"
    , "Property Insurance Rate [fraction / year]"
    ]
  tabulation Station{..} =
    [
      ""
    , show stationYear
    , show stationTotal
    , show stationOperating
    , show stationNew 
    , show stationElectricityUse
    , show stationNaturalGasUse
    , show stationDeliveredHydrogenUse
    , show stationCapacity
    , show stationCapitalCost
    , show stationInstallationCost
    , show stationIncidentalRevenue
    , show stationMaintenanceCost
    , show stationLicensingAndPermitting
    , show stationRentOfLand
    , show stationStaffing
    , show stationLaborRate
    , show stationSellingAndAdministrativeExpense
    , show stationCreditCardFeesRate
    , show stationSalesTaxRate
    , show stationRoadTax
    , show stationPropertyTaxRate
    , show stationPropertyInsuranceRate
    ]
  untabulation = undefined


costStation :: Quantity -> Operations -> Int -> Capital -> Capital
costStation quantity stationOperations@Operations{..} year stationCharacteristics@Station{..} =
  costOperations stationOperations year $
    stationCharacteristics
    {
      stationYear             = year
    , stationCapitalCost      = if quantity == 0 then stationCapitalCost else fromJust $ capitalCost quantity undefined year' stationCapacity
    , stationInstallationCost = if quantity == 0 then stationInstallationCost else fromJust $ installationCost quantity undefined year' stationCapacity
    , stationMaintenanceCost  = maintenanceCost stationCapacity * rMaintenance
    , stationRentOfLand       = rentCost stationCapacity * rRent
    }
    where
      year' = Year year
      rMaintenance =
        (1 + escalationRate stationMaintenanceCostFunction)
          **(fromIntegral year - independentStart stationMaintenanceCostFunction)
      rRent =
        (1 + escalationRate rentOfLandFunction)
          **(fromIntegral year - independentStart rentOfLandFunction)


data Operations =
  Operations
    {
      incidentalRevenueFunction             :: DoubleSeries --  $ / station / year
    , stationMaintenanceCostFunction        :: DoubleSeries --  $ / station / year
    , stationLicensingAndPermittingFunction :: DoubleSeries --  $ / station / year
    , rentOfLandFunction                    :: DoubleSeries --  $ / station / year
    , stationLaborRateFunction              :: DoubleSeries --  $ / station / hour
    , roadTaxFunction                       :: DoubleSeries --  $ / station / kg
    }
    deriving (Generic, Read, Show)

instance FromJSON Operations

instance ToJSON Operations where
  toJSON = genericToJSON defaultOptions


costOperations :: Operations -> Int -> Capital -> Capital
costOperations Operations{..} year stationCharacteristics@Station{..} =
  stationCharacteristics
  {
    stationIncidentalRevenue      = applyDefault stationIncidentalRevenue      incidentalRevenueFunction
  , stationMaintenanceCost        = applyDefault stationMaintenanceCost        stationMaintenanceCostFunction
  , stationLicensingAndPermitting = applyDefault stationLicensingAndPermitting stationLicensingAndPermittingFunction
  , stationRentOfLand             = applyDefault stationRentOfLand             rentOfLandFunction
  , stationLaborRate              = applyDefault stationLaborRate              stationLaborRateFunction
  , stationRoadTax                = applyDefault stationRoadTax                roadTaxFunction
  }
    where
      year' = fromIntegral year
      applyDefault :: Double -> DoubleSeries -> Double
      applyDefault value series =
        if isNaN value
          then asFunction series year'
          else value

scaleOperations :: Double -> Operations -> Operations
scaleOperations scale Operations{..} =
  Operations {
    incidentalRevenueFunction             = incidentalRevenueFunction             {dependentStart = scale * dependentStart incidentalRevenueFunction            }
  , stationMaintenanceCostFunction        = stationMaintenanceCostFunction        {dependentStart = scale * dependentStart stationMaintenanceCostFunction       }
  , stationLicensingAndPermittingFunction = stationLicensingAndPermittingFunction {dependentStart = scale * dependentStart stationLicensingAndPermittingFunction}
  , rentOfLandFunction                    = rentOfLandFunction                    {dependentStart = scale * dependentStart rentOfLandFunction                   }
  , stationLaborRateFunction              = stationLaborRateFunction              {dependentStart = scale * dependentStart stationLaborRateFunction             }
  , roadTaxFunction                       = roadTaxFunction                       {dependentStart = scale * dependentStart roadTaxFunction                      }
  }

ageStationTo :: Operations -> Capital -> Int -> [Capital]
ageStationTo stationOperations stationCharacteristics@Station{..} finalYear =
  stationCharacteristics
    : map (ageStation stationOperations stationCharacteristics) [(stationYear+1)..finalYear]


ageStation :: Operations -> Capital -> Int -> Capital
ageStation stationOperations stationCharacteristics year =
  (costOperations stationOperations year stationCharacteristics)
  {
    stationYear                   = year
  , stationOperating              = fromIntegral $ stationTotal stationCharacteristics -- FIXME: should account for installation
  , stationNew                    = 0
  , stationCapitalCost            = 0
  , stationInstallationCost       = 0
  }


combineStations :: [Capital] -> Capital
combineStations stations =
  let
    capacities = map stationCapacity stations
    capacity = sum capacities
    -- FIXME: The basis for these averages isn't quite correct.
    average f = sum (zipWith ((. f) . (*)) capacities stations) / capacity
    sum' f = sum (map f stations)
    average' f = f $ head stations
  in
    Station
    {
      stationYear                            = minimum (map stationYear stations)
    , stationTotal                           = sum' stationTotal
    , stationOperating                       = sum' stationOperating
    , stationNew                             = sum' stationNew
    , stationElectricityUse                  = average stationElectricityUse
    , stationNaturalGasUse                   = average stationNaturalGasUse
    , stationDeliveredHydrogenUse            = average stationDeliveredHydrogenUse
    , stationCapacity                        = capacity
    , stationCapitalCost                     = sum' stationCapitalCost
    , stationInstallationCost                = sum' stationInstallationCost
    , stationIncidentalRevenue               = sum' stationIncidentalRevenue
    , stationMaintenanceCost                 = sum' stationMaintenanceCost
    , stationLicensingAndPermitting          = sum' stationLicensingAndPermitting
    , stationRentOfLand                      = sum' stationRentOfLand
    , stationStaffing                        = sum' stationStaffing
    , stationLaborRate                       = average' stationLaborRate
    , stationSellingAndAdministrativeExpense = average stationSellingAndAdministrativeExpense
    , stationCreditCardFeesRate              = average stationCreditCardFeesRate
    , stationSalesTaxRate                    = average stationSalesTaxRate
    , stationRoadTax                         = average' stationRoadTax
    , stationPropertyTaxRate                 = average stationPropertyTaxRate
    , stationPropertyInsuranceRate           = average stationPropertyInsuranceRate
    }
