{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material.Types (
-- * Data types
  Material
, Zone
-- * Field types
, FMaterial
, FUpstreamMaterial
, FConsumptionRate
, FConsumptionRateStretch
, FProductionRate
, FProductionRateStretch
, FPrice
, FZone
, FTotalConsumption
, FTotalProduction
, FIntensity
-- * Field accessors
, fMaterial
, fUpstreamMaterial
, fConsumptionRate
, fConsumptionRateStretch
, fProductionRate
, fProductionRateStretch
, fPrice
, fZone
, fTotalConsumption
, fTotalProduction
, fIntensity
-- * Data cubes
, ConsumptionCube
, ProductionCube
, PriceCube
, ZoneCube
, IntensityCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "Material"                "Material"                          )
$(makeStringField "UpstreamMaterial"        "Upstream Material"                 )
$(makeField       "ConsumptionRate"         "Consumption [unit/kg]"     ''Double)
$(makeField       "ConsumptionRateStretch"  "Consumption [unit/km/kg]"  ''Double)
$(makeField       "ProductionRate"          "Production [unit/kg]"      ''Double)
$(makeField       "ProductionRateStretch"   "Production [unit/km/kg]"   ''Double)
$(makeField       "Price"                   "Price [$/unit]"            ''Double)
$(makeStringField "Zone"                    "Zone"                              )
$(makeField       "TotalConsumption"        "Consumption [unit]"        ''Double)
$(makeField       "TotalProduction"         "Production [unit]"         ''Double)
$(makeField       "Intensity"               "Intensity [upstream/unit]" ''Double)


type ConsumptionCube key = (FMaterial ': key) *↝ '[FConsumptionRate, FConsumptionRateStretch]


type ProductionCube key = (FMaterial ': key) *↝ '[FProductionRate, FProductionRateStretch]


type PriceCube key = (FMaterial ': key) *↝ '[FPrice]


type ZoneCube key = (FZone ': key) *↝ '[]


type IntensityCube = '[FMaterial, FUpstreamMaterial, FZone, FYear] *↝ '[FIntensity]
