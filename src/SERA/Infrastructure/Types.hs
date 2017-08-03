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
import SERA.Network.Types (FLength, FLocation, FSale, FTerritory, FX, FY)
import SERA.Process.Types (FCapacity, FCapitalCost, FCost, FFixedCost, FLifetime, FProductive, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)


data CostCategory =
    Capital
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

 
$(makeStringField "Infrastructure" "Infrastructure ID"                 )
$(makeField       "Production"     "Production [kg]"   ''Double        )
$(makeField       "Flow"           "Flow [kg]"         ''Double        )
$(makeField       "Loss"           "Loss [kg]"         ''Double        )
$(makeField       "Consumption"    "Consumption [kg]"  ''Double        )
$(makeField       "TotalCost"      "Cost [kg]"         ''Double        )
$(makeField       "Sales"          "Sales [$]"         ''Double        )
$(makeField       "NetPrice"       "Price [$/kg]"      ''Double        )
$(makeField       "Quantity"       "Quantity [unit]"   ''Double        )
$(makeField       "CostCategory"   "Cost Component"    ''CostCategory  )
$(makeField       "ImpactCategory" "Disposition"       ''ImpactCategory)
$(makeStringField "Position"       "Position"                          )


type DemandCube = '[FLocation, FYear] *↝ '[FConsumption]


type ConstructionCube = '[FInfrastructure] *↝ '[FLocation, FTechnology, FProductive, FYear, FLifetime, FCapacity, FLength, FCapitalCost, FFixedCost, FVariableCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FProduction, FFlow, FLoss, FSale]


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


type Construction = FieldRec '[FInfrastructure, FLocation, FTechnology, FProductive, FYear, FLifetime, FCapacity, FLength, FCapitalCost, FFixedCost, FVariableCost]


type Flow = FieldRec '[FInfrastructure, FYear, FProduction, FFlow, FLoss, FSale]


type Cash = FieldRec '[FInfrastructure, FYear, FCostCategory, FSale]


type Impact = FieldRec '[FInfrastructure, FYear, FMaterial, FImpactCategory, FQuantity, FSale]


type Geometry = FieldRec '[FLocation, FPosition, FX, FY]
