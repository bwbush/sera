{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material.Types (
-- * Data types
  Material
, UpstreamMaterial
, Pricer
-- * Field types
, FMaterial
, FUpstreamMaterial
, FConsumptionRate
, FConsumptionRateStretch
, FProductionRate
, FProductionRateStretch
, FPrice
, FBillable
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
, fBillable
, fTotalConsumption
, fTotalProduction
, fIntensity
-- * Data cubes
, ConsumptionCube
, ProductionCube
, PriceCube
, IntensityCube
-- * Key access
, materials
, upstreamMaterials
) where


import Data.Daft.DataCube (DataCube(Key, Keys))
import Data.Daft.Vinyl.FieldCube (FieldCube, type (*↝), υ)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set)
import Data.Vinyl.Lens (type (∈))
import SERA.Types (Year, FYear)
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "Material"                "Material"                          )
$(makeStringField "UpstreamMaterial"        "Upstream Material"                 )
$(makeField       "ConsumptionRate"         "Consumption [unit/kg]"     ''Double)
$(makeField       "ConsumptionRateStretch"  "Consumption [unit/km/kg]"  ''Double)
$(makeField       "ProductionRate"          "Production [unit/kg]"      ''Double)
$(makeField       "ProductionRateStretch"   "Production [unit/km/kg]"   ''Double)
$(makeField       "Price"                   "Price [$/unit]"            ''Double)
$(makeField       "Billable"                "Billable?"                 ''Bool  )
$(makeField       "TotalConsumption"        "Consumption [unit]"        ''Double)
$(makeField       "TotalProduction"         "Production [unit]"         ''Double)
$(makeField       "Intensity"               "Intensity [upstream/unit]" ''Double)


type ConsumptionCube key = (FMaterial ': key) *↝ '[FConsumptionRate, FConsumptionRateStretch]


type ProductionCube key = (FMaterial ': key) *↝ '[FProductionRate, FProductionRateStretch]


type PriceCube key = (FMaterial ': FYear ': key) *↝ '[FPrice, FBillable]


type IntensityCube key = (FMaterial ': FUpstreamMaterial ': FYear ': key) *↝ '[FIntensity]


materials :: (FMaterial ∈ ks, Key cube Material, DataCube cube, Keys cube ~ Set) => FieldCube cube ks vs -> Set Material
materials = υ (fMaterial <:)


upstreamMaterials :: (FUpstreamMaterial ∈ ks, Key cube UpstreamMaterial, DataCube cube, Keys cube ~ Set) => FieldCube cube ks vs -> Set UpstreamMaterial
upstreamMaterials = υ (fUpstreamMaterial <:)


type Pricer = Material -> Year -> Double
