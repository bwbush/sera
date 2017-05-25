{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material.Types (
-- * Field types
  FMaterial
, FConsumptionRate
, FProductionRate
, FPrice
, FZone
, FTotalConsumption
, FTotalProduction
-- * Field accessors
, fMaterial
, fConsumptionRate
, fProductionRate
, fPrice
, fZone
, fTotalConsumption
, fTotalProduction
-- * Data cubes
, ConsumptionCube
, ProductionCube
, PriceCube
, ZoneCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "Material"         "Material"                      )
$(makeField       "ConsumptionRate"  "Consumption [unit/kg]" ''Double)
$(makeField       "ProductionRate"   "Production [unit/kg]"  ''Double)
$(makeField       "Price"            "Price [$/unit]"        ''Double)
$(makeStringField "Zone"             "Zone"                          )
$(makeField       "TotalConsumption" "Consumption [unit]"    ''Double)
$(makeField       "TotalProduction"  "Production [unit]"     ''Double)


type ConsumptionCube key = (FMaterial ': key) *↝ '[FConsumptionRate]


type ProductionCube key = (FMaterial ': key) *↝ '[FProductionRate]


type PriceCube key = (FMaterial ': key) *↝ '[FPrice]


type ZoneCube key = (FZone ': key) *↝ '[]
