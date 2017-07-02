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
import SERA.Network.Types (FFrom, FLength, FLocation, FSale, FTerritory, FTo)
import SERA.Process.Types (FCapacity, FCapitalCost, FCost, FFixedCost, FLifetime, FProductive, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)


data CostCategory =
    Capital
  | Fixed
  | Variable
  | Consuming Material
  | Producing Material
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
$(makeField       "Sales"          "Sales [kg]"        ''Double        )
$(makeField       "NetPrice"       "Price [$/kg]"      ''Double        )
$(makeField       "Quantity"       "Quantity [unit]"   ''Double        )
$(makeField       "CostCategory"   "Cost Component"    ''CostCategory  )
$(makeField       "ImpactCategory" "Disposition"       ''ImpactCategory)


type DemandCube = '[FLocation, FYear] *↝ '[FConsumption]


type ConstructionCube = '[FInfrastructure] *↝ '[FFrom, FTo, FTechnology, FYear, FCapacity, FLength, FCapitalCost, FFixedCost, FVariableCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FProduction, FFlow, FLoss, FConsumption, FCost]


type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FCost]


type ImpactCube = '[FInfrastructure, FYear, FMaterial] *↝ '[FImpactCategory, FQuantity, FCost]


type SaleCube = '[FTerritory, FYear] *↝ '[FConsumption, FCost, FSales, FNetPrice]


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


type Construction = FieldRec '[FInfrastructure, FFrom, FTo, FTechnology, FProductive, FYear, FLifetime, FCapacity, FLength, FCapitalCost, FFixedCost, FVariableCost]


type Flow = FieldRec '[FInfrastructure, FYear, FProduction, FFlow, FLoss, FSale]


type Cash = FieldRec '[FInfrastructure, FYear, FCostCategory, FSale]


type Impact = FieldRec '[FInfrastructure, FYear, FMaterial, FImpactCategory, FQuantity, FSale]
