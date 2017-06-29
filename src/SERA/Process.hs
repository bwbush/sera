{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process (
  productions
, deliveries
, processes
, pathways
, ProcessSizer
, PathwaySizer
, sizeComponent
, sizePathway
) where


import Data.Daft.DataCube (selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, singleton, toList)
import Data.Vinyl.Derived (FieldRec)
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


type Technologies = Set (FieldRec '[FTechnology])


filterTechnologiesByProduction :: (Production -> Bool) -> ProcessLibrary -> [Technology]
filterTechnologiesByProduction f ProcessLibrary{..} =
  fmap (fTechnology <:)
    . toList
    $ (ω $ σ (const $ f . (fProduction <:)) processCostCube :: Technologies)


productions :: ProcessLibrary -> [Technology]
productions = filterTechnologiesByProduction isProduction


deliveries :: ProcessLibrary -> [Technology]
deliveries = filterTechnologiesByProduction $ not . isProduction


processes :: ProcessLibrary -> [Technology]
processes = filterTechnologiesByProduction $ const True


type Pathways = Set (FieldRec '[FPathway])


pathways :: ProcessLibrary -> [Pathway]
pathways ProcessLibrary{..} = fmap (fPathway <:) . toList $ (ω pathwayCube :: Pathways)


type ProcessSizer = Technology -> Year -> Double -> Double -> Maybe Component


sizeComponent :: ProcessLibrary -> ProcessSizer
sizeComponent ProcessLibrary{..} process year capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           process  == fTechnology <: key -- Technologies must match exactly,
        && year     <= fYear       <: key -- must be available in the given year, and
        && capacity <= fCapacity   <: key -- must be large enough.
    (specification, costs) <- selectKnownMaximum $ σ candidate processCostCube
    let
      component = Left process
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fCapacity <: specification) ** (fScaling <: costs)
      extract = κ (singleton specification) (const head) . σ (const . (specification ==) . τ)
      production = fProduction <: costs
      lifetime = fLifetime <: costs
      capitalCost = scaleCost fCapitalCost fCapitalCostStretch 
      fixedCost = scaleCost fFixedCost fFixedCostStretch
      variableCost = scaleCost fVariableCost fVariableCostStretch
      inputs = extract processInputCube
      outputs = extract processOutputCube
    return Component{..}


type PathwaySizer = Pathway -> Year -> Double -> Double -> Double -> Maybe Component


sizePathway :: ProcessLibrary -> PathwaySizer
sizePathway ProcessLibrary{..} pathway year capacity transmissionDistance deliveryDistance =
  do
    let
      candidate key _ = pathway == fPathway <: key
      processes' = σ candidate pathwayCube
    undefined
