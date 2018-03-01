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
, Cluster(..)
, Format(..)
, Geometry(..)
, ImpactCategory(..)
, Infrastructure(..)
, Location(..)
, Material(..)
, Pathway(..)
, Position(..)
, Productive(..)
, Region(..)
, Technology(..)
, Territory(..)
, UrbanCode(..)
, UrbanName(..)
, Year
, Zone(..)
-- * Field types
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
, FDutyCycle
, FExtended
, FFixedCost
, FFixedCostStretch
, FFlow
, FFormat
, FFraction
, FFrom
, FFuelConsumption
, FGeometry
, FImpactCategory
, FInfrastructure
, FIntensity
, FLength
, FLifetime
, FLocation
, FLoss
, FMaterial
, FNameplate
, FNetPrice
, FNonFuelConsumption
, FPathway
, FPosition
, FPrice
, FProduction
, FProductionRate
, FProductionRateStretch
, FProductive
, FQuantity
, FRegion
, FRent
, FSale
, FSales
, FSalvage
, FScaling
, FStage
, FTechnology
, FTerritory
, FTo
, FTotalConsumption
, FTotalCost
, FTotalProduction
, FTransmission
, FUpstreamMaterial
, FUrbanCode
, FUrbanName
, FVariableCost
, FVariableCostStretch
, FX
, FY
, FYear
, FYield
, FZone
-- * FIeld accessors
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
, fDutyCycle
, fExtended
, fFixedCost
, fFixedCostStretch
, fFlow
, fFormat
, fFraction
, fFrom
, fFuelConsumption
, fGeometry
, fImpactCategory
, fInfrastructure
, fIntensity
, fLength
, fLifetime
, fLocation
, fLoss
, fMaterial
, fNameplate
, fNetPrice
, fNonFuelConsumption
, fPathway
, fPosition
, fPrice
, fProduction
, fProductionRate
, fProductionRateStretch
, fProductive
, fQuantity
, fRegion
, fRent
, fSale
, fSales
, fSalvage
, fScaling
, fStage
, fTechnology
, fTerritory
, fTo
, fTotalConsumption
, fTotalCost
, fTotalProduction
, fTransmission
, fUpstreamMaterial
, fUrbanCode
, fUrbanName
, fVariableCost
, fVariableCostStretch
, fX
, fY
, fYear
, fYield
, fZone
) where


import Control.Arrow (first)
import SERA.Vehicle.Types (Age)
import SERA.Types.TH (makeField, makeStringField)


-- | Data type for calendar years.
type Year = Int


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
$(makeField "Year" "Year" ''Year)
$(makeStringField "UrbanCode" "Census Urban Area Code")
$(makeStringField "UrbanName" "Census Urban Area Name")
$(makeField "Fraction" "Fraction" ''Double)
$(makeField "Cluster" "Cluster ID" ''Cluster)
$(makeStringField "Geometry" "Geometry [WKT]")
