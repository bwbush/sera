{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Infrastructure.Types
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (Material, FMaterial)
import SERA.Network.Types (FLength, FInfrastructure, FLocation, FSale, FTerritory, FX, FY)
import SERA.Process.Types (FNameplate, FCapitalCost, FCost, FDutyCycle, FFixedCost, FLifetime, FProductive, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)


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

 
$(makeField       "Production"     "Production [kg]"   ''Double        )
$(makeField       "Flow"           "Flow [kg]"         ''Double        )
$(makeField       "Loss"           "Loss [kg]"         ''Double        )
$(makeField       "Consumption"    "Consumption [kg]"  ''Double        )
$(makeField       "FuelConsumption"    "Fuel Consumption [kg]"  ''Double        )
$(makeField       "NonFuelConsumption"    "Non-Fuel Consumption [kg]"  ''Double        )
$(makeField       "TotalCost"      "Cost [kg]"         ''Double        )
$(makeField       "Salvage"        "Salvage Value [$]" ''Double        )
$(makeField       "Sales"          "Sales [$]"         ''Double        )
$(makeField       "NetPrice"       "Price [$/kg]"      ''Double        )
$(makeField       "Quantity"       "Quantity [unit]"   ''Double        )
$(makeField       "CostCategory"   "Cost Component"    ''CostCategory  )
$(makeField       "ImpactCategory" "Disposition"       ''ImpactCategory)
$(makeStringField "Position"       "Position"                          )


type DemandCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption]


type ConstructionCube = '[FInfrastructure] *↝ '[FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FProduction, FFlow, FLoss, FSale, FSalvage]


type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FSale]


type ImpactCube = '[FInfrastructure, FYear, FMaterial, FImpactCategory] *↝ '[FQuantity, FSale]


type SaleCube = '[FTerritory, FYear] *↝ '[FProduction, FSale, FCost, FConsumption, FSales, FNetPrice]


type GeometryCube = '[FLocation, FPosition] *↝ '[FX, FY]


data InfrastructureCubes =
  InfrastructureCubes
  {
    constructionCube :: ConstructionCube
  , flowCube         :: FlowCube
  , cashCube         :: CashCube
  , impactCube       :: ImpactCube
  , saleCube         :: SaleCube
  }
    deriving (Eq, Ord, Show)


type Construction = FieldRec '[FInfrastructure, FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost]


type Flow = FieldRec '[FInfrastructure, FYear, FProduction, FFlow, FLoss, FSale, FSalvage]


type Cash = FieldRec '[FInfrastructure, FYear, FCostCategory, FSale]


type Impact = FieldRec '[FInfrastructure, FYear, FMaterial, FImpactCategory, FQuantity, FSale]


type Geometry = FieldRec '[FLocation, FPosition, FX, FY]
