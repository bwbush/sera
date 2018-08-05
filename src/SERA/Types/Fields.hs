-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types and accessors for data fields.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module SERA.Types.Fields (
-- * Data types
  CostCategory(..)
, Age
, AnnualTravel
, Cluster(..)
, Emission
, EmissionRate
, Energy
, Format(..)
, Fuel(..)
, FuelEfficiency
, FuelSplit
, Geometry(..)
, ImpactCategory(..)
, Infrastructure(..)
, Location(..)
, MarketShare
, Material(..)
, ModelYear
, OwnershipExpense(..)
, Pathway(..)
, Period(..)
, Pollutant
, Position(..)
, Productive(..)
, Purchases
, Region(..)
, RelativeMarketShare
, Stock
, Survival
, Technology(..)
, Territory(..)
, Travel
, UrbanCode(..)
, UrbanName(..)
, Vehicle(..)
, Vocation(..)
, Year
, Zone(..)
-- * Field types
, FAge
, FAnnualTravel
, FArea
, FBillable
, FCapacity
, FCapitalCost
, FCapitalCostStretch
, FCluster
, FCondition
, FConsumption
, FConsumptionRate
, FConsumptionRateStretch
, FCost
, FCostCategory
, FDelivery
, FDistance
, FDuration
, FDutyCycle
, FEmission
, FEmissionRate
, FEnergy
, FExtended
, FFixedCost
, FFixedCostStretch
, FFlow
, FFormat
, FFraction
, FFrom
, FFuel
, FFuelConsumption
, FFuelEfficiency
, FFuelSplit
, FGeometry
, FImpactCategory
, FInfrastructure
, FIntensity
, FLength
, FLifetime
, FLocation
, FLoss
, FMarketShare
, FMaterial
, FModelYear
, FNameplate
, FNetPrice
, FNonFuelConsumption
, FPathway
, FPeriod
, FPollutant
, FPosition
, FPrice
, FProduction
, FProductionRate
, FProductionRateStretch
, FProductive
, FPurchases
, FQuantity
, FRegion
, FWilderRegion
, FRelativeMarketShare
, FRent
, FSale
, FSales
, FSalvage
, FScaling
, FStage
, FStock
, FStorage
, FSurvival
, FTechnology
, FTerritory
, FTo
, FTotalConsumption
, FTotalCost
, FTotalProduction
, FTransmission
, FTravel
, FUpstreamMaterial
, FUrbanCode
, FUrbanName
, FVariableCost
, FVariableCostStretch
, FVehicle
, FVocation
, FX
, FY
, FYear
, FYield
, FZone
, FVehicleExpense
, FTravelExpense
, FFuelExpense
, FOwnershipExpense
-- * FIeld accessors
, fAge
, fAnnualTravel
, fArea
, fBillable
, fCapacity
, fCapitalCost
, fCapitalCostStretch
, fCluster
, fCondition
, fConsumption
, fConsumptionRate
, fConsumptionRateStretch
, fCost
, fCostCategory
, fDelivery
, fDistance
, fDuration
, fDutyCycle
, fEmission
, fEmissionRate
, fEnergy
, fExtended
, fFixedCost
, fFixedCostStretch
, fFlow
, fFormat
, fFraction
, fFrom
, fFuel
, fFuelConsumption
, fFuelEfficiency
, fFuelSplit
, fGeometry
, fImpactCategory
, fInfrastructure
, fIntensity
, fLength
, fLifetime
, fLocation
, fLoss
, fMarketShare
, fMaterial
, fModelYear
, fNameplate
, fNetPrice
, fNonFuelConsumption
, fPathway
, fPeriod
, fPollutant
, fPosition
, fPrice
, fProduction
, fProductionRate
, fProductionRateStretch
, fProductive
, fPurchases
, fQuantity
, fRegion
, fWilderRegion
, fRelativeMarketShare
, fRent
, fSale
, fSales
, fSalvage
, fScaling
, fStage
, fStock
, fStorage
, fSurvival
, fTechnology
, fTerritory
, fTo
, fTotalConsumption
, fTotalCost
, fTotalProduction
, fTransmission
, fTravel
, fUpstreamMaterial
, fUrbanCode
, fUrbanName
, fVariableCost
, fVariableCostStretch
, fVehicle
, fVocation
, fX
, fY
, fYear
, fYield
, fZone
, fVehicleExpense
, fTravelExpense
, fFuelExpense
, fOwnershipExpense
) where


import Control.Arrow (first)
import SERA.Types.TH (makeField, makeStringField, makeWilderField, makeWilderStringField)


-- | Data type for calendar years.
type Year = Int


-- | Data type for vehicle age.
type Age = Int



$(makeStringField "Material" "Material") 


newtype Cluster = Cluster {cluster :: Maybe Int}
  deriving (Eq, Ord)

instance Read Cluster where
  readsPrec n x =
    case first (Cluster . Just) <$> readsPrec n x of
      [] -> [(Cluster Nothing, x)]
      x' -> x'

instance Show Cluster where
  show (Cluster Nothing ) = ""
  show (Cluster (Just x)) = show x

data Productive =
    Onsite
  | Central
  | Yes
  | No
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


data CostCategory =
    Capital
  | Salvage
  | Fixed
  | Variable
  | Direct   Material
  | Indirect Material
    deriving (Eq, Ord, Read, Show)


data ImpactCategory =
    Consumption
  | Production
  | Upstream
    deriving (Eq, Ord, Read, Show)


$(makeField       "Consumption"            "Demand [kg]"                      ''Double        )
$(makeField       "CostCategory"           "Cost Component"                   ''CostCategory  )
$(makeField       "Flow"                   "Flow [kg]"                        ''Double        )
$(makeField       "FuelConsumption"        "Fueling-Station Demand [kg]"      ''Double        )
$(makeField       "ImpactCategory"         "Disposition"                      ''ImpactCategory)
$(makeField       "Loss"                   "Loss [kg]"                        ''Double        )
$(makeField       "NetPrice"               "Price [$/kg]"                     ''Double        )
$(makeField       "NonFuelConsumption"     "Non-Fueling-Station Demand [kg]"  ''Double        )
$(makeStringField "Position"               "Position"                                         )
$(makeField       "Production"             "Production [kg]"                  ''Double        )
$(makeField       "Quantity"               "Quantity [unit]"                  ''Double        )
$(makeField       "Sales"                  "Sales [$]"                        ''Double        )
$(makeField       "Salvage"                "Salvage Value [$]"                ''Double        )
$(makeField       "TotalCost"              "Cost [kg]"                        ''Double        )
$(makeField       "UpstreamMaterial"       "Upstream Material"                ''Material      )
$(makeField       "ConsumptionRate"        "Consumption [unit/kg]"            ''Double        )
$(makeField       "ConsumptionRateStretch" "Consumption [unit/km/kg]"         ''Double        )
$(makeField       "ProductionRate"         "Production [unit/kg]"             ''Double        )
$(makeField       "ProductionRateStretch"  "Production [unit/km/kg]"          ''Double        )
$(makeField       "Price"                  "Price [$/unit]"                   ''Double        )
$(makeField       "Billable"               "Billable?"                        ''Bool          )
$(makeField       "TotalConsumption"       "Consumption [unit]"               ''Double        )
$(makeField       "TotalProduction"        "Production [unit]"                ''Double        )
$(makeField       "Intensity"              "Intensity [upstream/unit]"        ''Double        )
$(makeStringField "Location"    "Network ID"                 )
$(makeField       "From"        "From Node ID"     ''Location)
$(makeField       "To"          "To Node ID"       ''Location)
$(makeField       "X"           "X"                ''Double  )
$(makeField       "Y"           "Y"                ''Double  )
$(makeField       "Area"        "Area [km^2]"      ''Double  )
$(makeField       "Length"      "Length [km]"      ''Double  )
$(makeField       "Sale"        "Cost [$]"         ''Double  )
$(makeField       "Rent"        "Cost [$/yr]"      ''Double  )
$(makeStringField "Territory"   "Territory"                  )
$(makeStringField "Zone"        "Zone"                       )
$(makeStringField "Infrastructure" "Infrastructure ID"                 )
$(makeStringField "Technology"          "Technology"                                    )
$(makeField       "Distance"            "Distance [km]"                     ''Double    )
$(makeField       "Capacity"            "Capacity [kg/yr]"                  ''Double    )
$(makeField       "Nameplate"           "Nameplate Capacity [kg/yr]"        ''Double    )
$(makeField       "Productive"          "Production?"                       ''Productive)
$(makeField       "Lifetime"            "Lifetime [yr]"                     ''Age       )
$(makeField       "Scaling"             "Scaling Exponent"                  ''Double    )
$(makeField       "CapitalCost"         "Capital Cost [$]"                  ''Double    )
$(makeField       "CapitalCostStretch"  "Capital Cost [$/km]"               ''Double    )
$(makeField       "FixedCost"           "Fixed Operating Cost [$/yr]"       ''Double    )
$(makeField       "FixedCostStretch"    "Fixed Operating Cost [$/km/yr]"    ''Double    )
$(makeField       "VariableCost"        "Variable Operating Cost [$/kg]"    ''Double    )
$(makeField       "VariableCostStretch" "Variable Operating Cost [$/km/kg]" ''Double    )
$(makeField       "DutyCycle"           "Maximum Utilization [kg/kg]"       ''Double    )
$(makeStringField "Pathway"             "Pathway"                                       )
$(makeField       "Stage"               "Stage"                             ''Int       )
$(makeField       "Extended"            "Extended?"                         ''Bool      )
$(makeField       "Transmission"        "Transmission?"                     ''Bool      )
$(makeField       "Delivery"            "Delivery?"                         ''Bool      )
$(makeStringField "Format"              "Format"                                        )
$(makeField       "Cost"                "Cost [$/kg]"                       ''Double    )
$(makeField       "Yield"               "Yield [upstream/kg]"               ''Double    )
$(makeField       "Condition"           "Condition?"                        ''Bool      )
$(makeStringField "Region" "Region")
$(makeWilderField "WilderRegion" "Region" ''Region)
$(makeField "Year" "Year" ''Year)
$(makeStringField "UrbanCode" "Census Urban Area Code")
$(makeStringField "UrbanName" "Census Urban Area Name")
$(makeField "Fraction" "Fraction" ''Double)
$(makeField "Cluster" "Cluster ID" ''Cluster)
$(makeStringField "Geometry" "Geometry [WKT]")


-- | Data type for vehicle vocation.
-- | Field type for vehicle vocation.
-- | Field label for vehicle vocation.
$(makeWilderStringField "Vocation" "Vocation")


-- | Data type for vehicle type.
-- | Field type for vehicle type.
-- | Field label for vehicle type.
$(makeWilderStringField "Vehicle" "Vehicle")


-- | Field type for vehicle age.
-- | Field label for vehicle age.
$(makeWilderField "Age" "Age [yr]" ''Age)


-- | Data type for model year.
type ModelYear = Int


-- | Field type for model year.
$(makeWilderField "ModelYear" "Model Year" ''ModelYear)


-- | Field label for model year.
-- | Data type for fuel.
-- | Field type for fuel.
-- | Field label for fuel.
$(makeStringField "Fuel" "Fuel")


-- | Data type for pollutant type.
-- | Field type for pollutant type.
-- | Field label for pollutant type.
$(makeStringField "Pollutant" "Pollutant")


-- | Data type for market share.
type MarketShare = Double


-- | Field type for market share.
-- | Field label for market share.
$(makeField "MarketShare" "Market Share [veh/veh]" ''MarketShare)


-- | Data type for relative market share.
type RelativeMarketShare = Double


-- | Field type for relative market share.
-- | Field label for relative market share.
$(makeField "RelativeMarketShare" "Relative Market Share" ''RelativeMarketShare)


-- | Data type for vehicle survival.
type Survival = Double


-- | Field type for vehicle survival.
-- | Field label for vehicle survival.
$(makeField "Survival" "Surviving Vehicles [veh/veh]" ''Survival)


-- | Data type for annual travel.
type AnnualTravel = Double


-- | Field type for annual travel.
-- | Field label for annual travel.
$(makeField "AnnualTravel" "Annual Travel [mi/yr]" ''AnnualTravel)


-- | Data type for fuel split.
type FuelSplit = Double


-- | Field type for fuel split.
-- | Field label for fuel split.
$(makeField "FuelSplit" "Fraction of Travel [mi/mi]" ''FuelSplit)


-- | Data type for fuel efficiency.
type FuelEfficiency = Double


-- | Field type for fuel efficiency.
-- | Field label for fuel efficiency.
$(makeField "FuelEfficiency" "Fuel Efficiency [mi/gge]" ''FuelEfficiency)


-- | Data type for emission rate.
type EmissionRate = Double


-- | Field type for emission rate.
-- | Field label for emission rate.
$(makeField "EmissionRate" "Emission Rate [g/gge]" ''EmissionRate)


-- | Data type for vehicle sales.
type Purchases = Double


-- | Field type for vehicle sales.
-- | Field label for vehicle sales.
$(makeField "Purchases" "Sales [veh]" ''Purchases)


-- | Data type for vehicle stock.
type Stock = Double


-- | Field type for vehicle stock.
-- | Field label for vehicle stock.
$(makeField "Stock" "Stock [veh]" ''Stock)


-- | Data type for distance traveled.
type Travel = Double


-- | Field type for distance traveled.
-- | Field label for distance traveled.
$(makeField "Travel" "Travel [mi]" ''Travel)


-- | Data type for energy consumed.
type Energy = Double


-- | Field type for energy consumed.
-- | Field label for energy consumed.
$(makeField "Energy" "Energy [gge]" ''Energy)


-- | Data type for pollutants emitted.
type Emission = Double


-- | Field type for pollutants emitted.
-- | Field label for pollutants emitted.
$(makeField "Emission" "Emission [g]" ''Emission)


$(makeField "VehicleExpense" "Cost [$/veh]" ''Double)
$(makeField "TravelExpense"  "Cost [$/mi]"  ''Double)
$(makeField "FuelExpense"    "Cost [$/gge]" ''Double)

data OwnershipExpense =
    PerVehicle
  | PerMile
  | PerGGE Fuel
    deriving (Eq, Ord, Read, Show)

$(makeField "OwnershipExpense"    "Expense Type" ''OwnershipExpense)



$(makeField "Storage" "Storage Capacity [kg]" ''Double)


$(makeStringField "Period" "Period") 


$(makeField "Duration" "Duration [yr/yr]" ''Double)
