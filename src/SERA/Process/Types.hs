{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Process.Types
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (type (*↝), σ, υ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (ConsumptionCube, ProductionCube)
import SERA.Types (FYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types (Age)

import qualified Data.Set as S (map)


data Productive =
    Onsite
  | Central
  | No
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


isProduction :: Productive -> Bool
isProduction No = False
isProduction _  = True


-- TO DO: eliminate distance and add linear distance scaling, also add capacity scaling exponent.
$(makeStringField "Technology"          "Technology"                                    )
$(makeField       "Distance"            "Distance [km]"                     ''Double    )
$(makeField       "Capacity"            "Capacity [kg/yr]"                  ''Double    )
$(makeField       "Productive"          "Production?"                       ''Productive)
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
$(makeField       "Extended"            "Extended?"                         ''Bool      )
$(makeField       "Transmission"        "Transmission?"                     ''Bool      )
$(makeField       "Delivery"            "Delivery?"                         ''Bool      )
$(makeStringField "Format"              "Format"                                        )
$(makeField       "Cost"                "Cost [$/kg]"                       ''Double    )
$(makeField       "Yield"               "Yield [upstream/kg]"               ''Double    )


type ProcessKey = '[FTechnology, FYear, FCapacity]


type ProcessCost = '[FProductive, FLifetime, FScaling, FCapitalCost, FCapitalCostStretch, FFixedCost, FFixedCostStretch, FVariableCost, FVariableCostStretch]


type ProcessCostCube = ProcessKey *↝ ProcessCost


type ProcessInputCube = ConsumptionCube ProcessKey


type ProcessOutputCube = ProductionCube ProcessKey


type PathwayCube = '[FPathway, FStage] *↝  '[FTechnology, FYield, FExtended, FTransmission, FDelivery, FFormat]


data ProcessLibrary =
  ProcessLibrary
  {
    processCostCube   :: ProcessCostCube
  , processInputCube  :: ProcessInputCube
  , processOutputCube :: ProcessOutputCube
  , pathwayCube       :: PathwayCube
  }
    deriving (Eq, Ord, Show)


type Technologies = Set (FieldRec '[FTechnology])


filterTechnologiesByProductive :: (Productive -> Bool) -> ProcessLibrary -> Set Technology
filterTechnologiesByProductive f ProcessLibrary{..} =
  S.map (fTechnology <:)
    $ (ω $ σ (const $ f . (fProductive <:)) processCostCube :: Technologies)


productions :: ProcessLibrary -> Set Technology
productions = filterTechnologiesByProductive isProduction


productions' :: Productive -> ProcessLibrary -> Set Technology
productions' = filterTechnologiesByProductive . (==)


deliveries :: ProcessLibrary -> Set Technology
deliveries = filterTechnologiesByProductive $ not . isProduction


processes :: ProcessLibrary -> Set Technology
processes = filterTechnologiesByProductive $ const True


pathways :: ProcessLibrary -> Set Pathway
pathways ProcessLibrary{..} = υ (fPathway <:) pathwayCube
