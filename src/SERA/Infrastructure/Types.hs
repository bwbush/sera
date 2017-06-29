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
import SERA.Material.Types (Material, FMaterial)
import SERA.Network.Types (FFrom, FLength, FLocation, FTerritory, FTo)
import SERA.Process.Types (FCapacity, FCapitalCost, FCost, FFixedCost, FProduction, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)


data CostCategory =
    Capital
  | Fixed
  | Operating
  | MaterialCategory Material
    deriving (Eq, Ord, Read, Show)

 
$(makeStringField "Infrastructure" "Infrastructure ID"         )
$(makeField       "Flow"           "Flow [kg]"         ''Double)
$(makeField       "Loss"           "Loss [kg]"         ''Double)
$(makeField       "Consumption"    "Consumption [kg]"  ''Double)
$(makeField       "TotalCost"      "Cost [kg]"         ''Double)
$(makeField       "Sales"          "Sales [kg]"        ''Double)
$(makeField       "NetPrice"       "Price [$/kg]"      ''Double)
$(makeField       "Emission"       "Emission [unit]"   ''Double)
$(makeField       "CostCategory"   "Cost Category"     ''Double)


type DemandCube = '[FLocation, FYear] *↝ '[FConsumption]


type ConstructionCube = '[FInfrastructure] *↝ '[FFrom, FTo, FTechnology, FYear, FCapacity, FLength, FCapitalCost, FFixedCost, FVariableCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FProduction, FFlow, FLoss, FConsumption, FCost]


type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FCost]


type ImpactCube = '[FInfrastructure, FYear, FMaterial] *↝ '[FEmission, FCost]


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
