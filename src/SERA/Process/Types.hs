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


data Production =
    Onsite
  | Central
  | No
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


isProduction :: Production -> Bool
isProduction No = False
isProduction _  = True


-- TO DO: eliminate distance and add linear distance scaling, also add capacity scaling exponent.
$(makeStringField "Technology"          "Technology"                                    )
$(makeField       "Distance"            "Distance [km]"                     ''Double    )
$(makeField       "Capacity"            "Capacity [kg/yr]"                  ''Double    )
$(makeField       "Production"          "Production?"                       ''Production)
$(makeField       "Lifetime"            "Lifetime [yr]"                     ''Age       )
$(makeField       "Scaling"             "Scaling Exponent"                  ''Double    )
$(makeField       "CapitalCost"         "Capital Cost [$]"                  ''Double    )
$(makeField       "CapitalCostStretch"  "Capital Cost [$/km]"               ''Double    )
$(makeField       "FixedCost"           "Fixed Operating Cost [$/yr]"       ''Double    )
$(makeField       "FixedCostStretch"    "Fixed Operating Cost [$/km/yr]"    ''Double    )
$(makeField       "VariableCost"        "Variable Operating Cost [$/kg]"    ''Double    )
$(makeField       "VariableCostStretch" "Variable Operating Cost [$/km/kg]" ''Double    )
$(makeStringField "Pathway"             "Pathway"                                       )
$(makeField       "Stage"               "Stage"                             ''Int       )
$(makeField       "Transmission"        "Transmission?"                     ''Bool      )
$(makeField       "Delivery"            "Delivery?"                         ''Bool      )
$(makeField       "Cost"                "Cost [$/kg]"                       ''Double    )
$(makeField       "Yield"               "Yield [upstream/kg]"               ''Double    )


type ProcessKey = '[FTechnology, FYear, FCapacity]


type ProcessCost = '[FProduction, FLifetime, FScaling, FCapitalCost, FCapitalCostStretch, FFixedCost, FFixedCostStretch, FVariableCost, FVariableCostStretch]


type ProcessCostCube = ProcessKey *↝ ProcessCost


type ProcessInputCube = ConsumptionCube ProcessKey


type ProcessOutputCube = ProductionCube ProcessKey


type PathwayCube = '[FPathway, FTechnology] *↝  '[FStage, FTransmission, FDelivery]


data ProcessLibrary =
  ProcessLibrary
  {
    processCostCube   :: ProcessCostCube
  , processInputCube  :: ProcessInputCube
  , processOutputCube :: ProcessOutputCube
  , pathwayCube       :: PathwayCube
  }
    deriving (Eq, Ord, Show)


data Component =
    TechnologyComponent
    {
      tech         :: Technology
    , year         :: Year
    , capacity     :: Double
    , production   :: Production
    , lifetime     :: Age
    , capitalCost  :: Double
    , fixedCost    :: Double
    , variableCost :: Double
    , inputs       :: ConsumptionCube '[] -- FIXME: Memoize this.
    , outputs      :: ProductionCube '[]  -- FIXME: Memoize this.
    }
  | PathwayComponent
    {
      path         :: Pathway
    , year         :: Year
    , capacity     :: Double
    , distance     :: Double
    , production   :: Production
    , lifetime     :: Age
    , capitalCost  :: Double
    , fixedCost    :: Double
    , variableCost :: Double
    , inputs       :: ConsumptionCube '[] -- FIXME: Memoize this.
    , outputs      :: ProductionCube '[]  -- FIXME: Memoize this.
    }
    deriving (Eq, Ord, Show)
