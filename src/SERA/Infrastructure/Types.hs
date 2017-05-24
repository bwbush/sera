{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Infrastructure.Types
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (type (↝))
import SERA.Material.Types (FConsumptionRate, FMaterial, FProductionRate)
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


type ProcessKey = '[FTechnology, FDistance, FCapacity, FYear]


type ProcessCube = ProcessKey ↝ '[FOnSite, FLifetime]


type ProcessCostCube = ProcessKey ↝ '[FCapitalCost, FFixedOperatingCost, FVariableOperatingCost, FYield]


type ProcessInputCube = (FMaterial ': ProcessKey) ↝ '[FConsumptionRate]


type ProcessOutputCube = (FMaterial ': ProcessKey) ↝ '[FProductionRate]
