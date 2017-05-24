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
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types (Age, FPollutant)


$(makeStringField "Technology"            "Technology"                             )
$(makeField       "Distance"              "Distance [km]"                  ''Double)
$(makeField       "Capacity"              "Capacity [kg/yr]"               ''Double)
$(makeField       "CapitalCost"           "Capital Cost [$]"               ''Double)
$(makeField       "FixedOperatingCost"    "Fixed Operating Cost [$/yr]"    ''Double)
$(makeField       "VariableOperatingCost" "Variable Operating Cost [$/kg]" ''Double)
$(makeStringField "Feedstock"             "Feedstock"                              )
$(makeField       "Yield"                 "Yield [kg/kg]"                  ''Double)
$(makeField       "Efficiency"            "Efficiency [kg/]"               ''Double)
$(makeField       "Consumption"           "Consumption [/kg]"              ''Double)
$(makeField       "FeedstockPrice"        "Feedstock Price [$/]"           ''Double)
$(makeField       "OnSite"                "On Site?"                       ''Bool  )
$(makeField       "Lifetime"              "Lifetime [yr]"                  ''Age   )
$(makeField       "Emission"              "Emission [/kg]"                 ''Double)


type ProcessKey = '[FTechnology, FDistance, FCapacity, FYear]


type ProcessCube = ProcessKey ↝ '[FOnSite, FLifetime]


type ProcessCostCube = ProcessKey ↝ '[FCapitalCost, FFixedOperatingCost, FVariableOperatingCost, FYield]


type ProcessInputCube = ProcessKey ↝ '[FFeedstock, FConsumption]


type ProcessOutputCube = ProcessKey ↝ '[FPollutant, FEmission]
