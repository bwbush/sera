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
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (ConsumptionCube, ProductionCube)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types (Age)


$(makeStringField "Technology"            "Technology"                             )
$(makeField       "Distance"              "Distance [km]"                  ''Double)
$(makeField       "Capacity"              "Capacity [kg/yr]"               ''Double)
$(makeField       "CapitalCost"           "Capital Cost [$]"               ''Double)
$(makeField       "FixedOperatingCost"    "Fixed Operating Cost [$/yr]"    ''Double)
$(makeField       "VariableOperatingCost" "Variable Operating Cost [$/kg]" ''Double)
$(makeField       "Yield"                 "Yield [kg/kg]"                  ''Double)
$(makeField       "OnSite"                "On Site?"                       ''Bool  )
$(makeField       "Lifetime"              "Lifetime [yr]"                  ''Age   )


type ProcessKey = '[FTechnology, FYear, FCapacity, FDistance]


type ProcessProperties = '[FOnSite, FLifetime]


type ProcessCube = ProcessKey *↝ ProcessProperties


type ProcessCost = '[FCapitalCost, FFixedOperatingCost, FVariableOperatingCost, FYield]


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
    componentSpecification :: FieldRec ProcessKey
  , componentProperties    :: FieldRec ProcessProperties
  , componentCosts         :: FieldRec ProcessCost
  , componentInputs        :: ConsumptionCube '[]
  , componentOutputs       :: ProductionCube '[]
  }
    deriving (Eq, Ord, Show)
