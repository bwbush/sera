{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Technology
-- FIXME
where


import Data.Daft.DataCube (selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (singleton)
import SERA.Infrastructure.Types -- FIXME
import SERA.Material.Types -- FIXME
import SERA.Network.Types -- FIXME
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


type TechnologyOperation = Year -> Double -> (Flow, [Cash], [Impact])


type TechnologyReifier = Technology -> Year -> Double -> Double -> Maybe (Construction, TechnologyOperation)


technologyReifier :: ProcessLibrary -> Pricer -> TechnologyReifier
technologyReifier ProcessLibrary{..} pricer tech built capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           tech     == fTechnology <: key -- Technologies must match exactly,
        && built    >= fYear       <: key -- must be available in the given built, and
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
        <+> fYear         =: built
        <+> fLifetime     =: fLifetime <: costs
        <+> fCapacity     =: capacity
        <+> fLength       =: distance
        <+> fCapitalCost  =: scaleCost fCapitalCost fCapitalCostStretch 
        <+> fFixedCost    =: scaleCost fFixedCost fFixedCostStretch
        <+> fVariableCost =: scaleCost fVariableCost fVariableCostStretch
      inputs = extract processInputCube :: ConsumptionCube '[]
      outputs = extract processOutputCube :: ProductionCube '[]
    return
      (
        construction
      , \year output ->
        let
          capital = if built == year then fCapitalCost <: construction else 0
          fixed = fFixedCost <: construction
          variable = output * fVariableCost <: construction
          impacts =
            [
                  fMaterial       =: fMaterial <: rec
              <+> fImpactCategory =: Consumption
              <+> fQuantity       =: output * rate
              <+> fSale           =: output * rate * pricer (fMaterial <: rec) year
            |
              rec <- toKnownRecords inputs
            , let rate = fConsumptionRate <: rec + distance * fConsumptionRateStretch <: rec
            ]
            ++
            [
                  fMaterial       =: fMaterial <: rec
              <+> fImpactCategory =: Production
              <+> fQuantity       =: output * rate
              <+> fSale           =: output * rate * pricer (fMaterial <: rec) year
            |
              rec <- toKnownRecords outputs
            , let rate = fProductionRate <: rec + distance * fProductionRateStretch <: rec
            ]
        in
          (
                fProduction =: (if isProduction (fProductive <: costs) then output else 0       )
            <+> fFlow       =: (if isProduction (fProductive <: costs) then 0        else output)
            <+> fLoss       =: 0
            <+> fSale       =: 0
          , [
              fCostCategory =: Capital  <+> fSale =: capital
            , fCostCategory =: Fixed    <+> fSale =: fixed
            , fCostCategory =: Variable <+> fSale =: variable
            ]
            ++
            [
              fCostCategory =: cc (fMaterial <: rec) <+> fSale =: fSale <: rec
            |
              rec <- impacts
            , let cc = case fImpactCategory <: rec of
                         Consumption -> Consuming
                         Production  -> Producing
                         Upstream    -> Producing
            ]
          , impacts
          )
      )
