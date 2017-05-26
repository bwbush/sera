{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Process.Types
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Material.Types (ConsumptionCube, ProductionCube)
import SERA.Types (Year, FYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types (Age)


-- TO DO: eliminate distance and add linear distance scaling, also add capacity scaling exponent.
$(makeStringField "Technology"          "Technology"                                )
$(makeField       "Distance"            "Distance [km]"                     ''Double)
$(makeField       "Capacity"            "Capacity [kg/yr]"                  ''Double)
$(makeField       "Scaling"             "Scaling Exponent"                  ''Double)
$(makeField       "CapitalCost"         "Capital Cost [$]"                  ''Double)
$(makeField       "CapitalCostStretch"  "Capital Cost [$/km]"               ''Double)
$(makeField       "FixedCost"           "Fixed Operating Cost [$/yr]"       ''Double)
$(makeField       "FixedCostStretch"    "Fixed Operating Cost [$/km/yr]"    ''Double)
$(makeField       "VariableCost"        "Variable Operating Cost [$/kg]"    ''Double)
$(makeField       "VariableCostStretch" "Variable Operating Cost [$/km/kg]" ''Double)
$(makeField       "Yield"               "Yield [kg/kg]"                     ''Double)
$(makeField       "OnSite"              "On Site?"                          ''Bool  )
$(makeField       "Lifetime"            "Lifetime [yr]"                     ''Age   )


type ProcessKey = '[FTechnology, FYear, FCapacity]


type ProcessProperties = '[FOnSite, FLifetime]


type ProcessCube = ProcessKey *↝ ProcessProperties


type ProcessCost = '[FYield, FScaling, FCapitalCost, FCapitalCostStretch, FFixedCost, FFixedCostStretch, FVariableCost, FVariableCostStretch]


type ProcessCostCube = ProcessKey *↝ ProcessCost


type ProcessInputCube = ConsumptionCube ProcessKey


type ProcessOutputCube = ProductionCube ProcessKey


data ProcessLibrary =
  ProcessLibrary
  {
    processCube       :: ProcessCube
  , processCostCube   :: ProcessCostCube
  , processInputCube  :: ProcessInputCube
  , processOutputCube :: ProcessOutputCube
  }
    deriving (Eq, Ord, Show)


data Component =
  Component
  {
    component    :: Technology
  , year         :: Year
  , capacity     :: Double
  , distance     :: Double
  , onSite       :: Bool
  , lifetime     :: Age
  , yield        :: Double
  , capitalCost  :: Double
  , fixedCost    :: Double
  , variableCost :: Double
  , inputs       :: ConsumptionCube '[] -- FIXME: Memoize this.
  , outputs      :: ProductionCube '[]  -- FIXME: Memoize this.
  }
    deriving (Eq, Ord, Show)
