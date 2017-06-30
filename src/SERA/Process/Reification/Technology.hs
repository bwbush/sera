{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Technology
-- FIXME
where


import Data.Daft.DataCube (selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (Set, singleton, toList)
import SERA.Infrastructure.Types -- FIXME
import SERA.Material.Types -- FIXME
import SERA.Network.Types -- FIXME
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


type TechnologyReifier = Technology -> Year -> Double -> Double -> Maybe (Construction)


technologyReifier :: ProcessLibrary -> PriceCube '[] -> TechnologyReifier
technologyReifier ProcessLibrary{..} prices tech year capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           tech     == fTechnology <: key -- Technologies must match exactly,
        && year     >= fYear       <: key -- must be available in the given year, and
        && capacity >= fCapacity   <: key -- must be large enough.
    (specification, costs) <- selectKnownMaximum $ σ candidate processCostCube
    let
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fCapacity <: specification) ** (fScaling <: costs)
      extract = κ (singleton specification) (const head) . σ (const . (specification ==) . τ)
      construction =
            fTechnology   =: tech
        <+> fProductive   =: fProductive <: costs
        <+> fYear         =: year
        <+> fLifetime     =: fLifetime <: costs
        <+> fCapacity     =: capacity
        <+> fLength       =: distance
        <+> fCapitalCost  =: scaleCost fCapitalCost fCapitalCostStretch 
        <+> fFixedCost    =: scaleCost fFixedCost fFixedCostStretch
        <+> fVariableCost =: scaleCost fVariableCost fVariableCostStretch
      inputs = extract processInputCube :: ConsumptionCube '[]
      outputs = extract processOutputCube :: ProductionCube '[]
    return construction
