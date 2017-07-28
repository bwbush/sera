{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Technology
-- FIXME
where


import Data.Daft.DataCube (knownKeys, selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube ((!), σ, τ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec)
import SERA.Infrastructure.Types -- FIXME
import SERA.Material.Types -- FIXME
import SERA.Network.Types -- FIXME
import SERA.Process.Types -- FIXME
import SERA.Types (Year, FYear, fYear)

import qualified Data.Set as S (filter, findMin, map, null, toList)


type TechnologyOperation = Year -> Double -> (Flow, [Cash], [Impact])


type TechnologyReifier = FieldRec '[FInfrastructure, FLocation] -> Year -> Double -> Double -> Technology -> Maybe (Construction, TechnologyOperation)


technologyReifier :: ProcessLibrary -> IntensityCube '[] -> Pricer -> TechnologyReifier
technologyReifier ProcessLibrary{..} intensityCube pricer specifics built capacity distance tech = 
  do -- FIXME: Add interpolation
    let
      eligible =
        [
              fTechnology =: tech
          <+> fYear       =: year
          <+> (if null caps' then minimum caps else maximum caps')
        |
          let keys = S.filter (\key -> tech == fTechnology <: key && built >= fYear <: key) $ knownKeys processCostCube
        , let year = (fYear <:) . S.findMin $ S.map (\key -> τ key :: FieldRec '[FYear]) keys
        , let caps = S.map (\key -> τ key :: FieldRec '[FCapacity]) $ S.filter (\key -> year == fYear <: key) keys
        , let caps' = S.filter (\key -> capacity >= fCapacity <: key) caps
        ]
    (specification, costs) <- selectKnownMaximum $ σ (\key _ -> key `elem` eligible) processCostCube
    let
      capacity' = maximum [capacity, fCapacity <: specification]
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity' / fCapacity <: specification) ** (fScaling <: costs)
      construction =
            specifics
        <+> fTechnology   =: tech
        <+> fProductive   =: fProductive <: costs
        <+> fYear         =: built
        <+> fLifetime     =: fLifetime <: costs
        <+> fCapacity     =: capacity'
        <+> fLength       =: distance
        <+> fCapitalCost  =: scaleCost fCapitalCost fCapitalCostStretch 
        <+> fFixedCost    =: scaleCost fFixedCost fFixedCostStretch
        <+> fVariableCost =: fVariableCost <: costs + distance * fVariableCostStretch <: costs
      inputs =
        σ (\kex _ -> kex `elem` [
                                      fMaterial   =: material
                                  <+> fTechnology =: tech
                                  <+> fYear       =: year
                                  <+> (if null caps' then minimum caps else maximum caps')
                                |
                                  let keys = S.filter (\key -> tech == fTechnology <: key && built >= fYear <: key) $ knownKeys processInputCube
                                , material <- S.toList $ S.map (fMaterial <:) keys
                                , let keys' = S.filter (\key -> material == fMaterial <: key) keys
                                , let year = (fYear <:) . S.findMin $ S.map (\key -> τ key :: FieldRec '[FYear]) keys'
                                , let caps = S.map (\key -> τ key :: FieldRec '[FCapacity]) $ S.filter (\key -> year == fYear <: key) keys'
                                , let caps' = S.filter (\key -> capacity' >= fCapacity <: key) caps
                                ]
          ) processInputCube
      outputs =
        σ (\kex _ -> kex `elem` [
                                      fMaterial   =: material
                                  <+> fTechnology =: tech
                                  <+> fYear       =: year
                                  <+> (if null caps' then minimum caps else maximum caps')
                                |
                                  let keys = S.filter (\key -> tech == fTechnology <: key && built >= fYear <: key) $ knownKeys processOutputCube
                                , material <- S.toList $ S.map (fMaterial <:) keys
                                , let keys' = S.filter (\key -> material == fMaterial <: key) keys
                                , let year = (fYear <:) . S.findMin $ S.map (\key -> τ key :: FieldRec '[FYear]) keys'
                                , let caps = S.map (\key -> τ key :: FieldRec '[FCapacity]) $ S.filter (\key -> year == fYear <: key) keys'
                                , let caps' = S.filter (\key -> capacity' >= fCapacity <: key) caps
                                ]
          ) processOutputCube
    return
      (
        construction
      , \year output ->
        let
          specifics' = fInfrastructure =: fInfrastructure <: specifics <+> fYear =: year
          capital = if built == year then fCapitalCost <: construction else 0
          fixed = fFixedCost <: construction
          variable = output * fVariableCost <: construction
          impacts =
            [
                  specifics'
              <+> fMaterial       =: upstream
              <+> fImpactCategory =: Upstream
              <+> fQuantity       =: quantity * intensity
              <+> fSale           =: maximum [0, - quantity * intensity * pricer upstream year]
            |
              rec <- toKnownRecords inputs
            , let material = fMaterial <: rec
            , let rate = fConsumptionRate <: rec + distance * fConsumptionRateStretch <: rec
            , let quantity = output * rate
            , upstream <- S.toList $ upstreamMaterials intensityCube :: [Material]
            , let keys = S.filter (\key -> material == fMaterial <: key && upstream == fUpstreamMaterial <: key && year >= fYear <: key) $ knownKeys intensityCube :: Set (FieldRec '[FMaterial, FUpstreamMaterial, FYear])
            , not $ S.null keys
            , let intensity = (fIntensity <:) $ intensityCube ! S.findMin keys :: Double
            ]
            ++
            [
                  specifics'
              <+> fMaterial       =: fMaterial <: rec
              <+> fImpactCategory =: Consumption
              <+> fQuantity       =: - output * rate
              <+> fSale           =: output * rate * pricer (fMaterial <: rec) year
            |
              rec <- toKnownRecords inputs
            , let rate = fConsumptionRate <: rec + distance * fConsumptionRateStretch <: rec
            ]
            ++
            [
                  specifics'
              <+> fMaterial       =: fMaterial <: rec
              <+> fImpactCategory =: Production
              <+> fQuantity       =: output * rate
              <+> fSale           =: - output * rate * pricer (fMaterial <: rec) year
            |
              rec <- toKnownRecords outputs
            , let rate = fProductionRate <: rec + distance * fProductionRateStretch <: rec
            ]
          cash =
            [
              specifics' <+> fCostCategory =: Capital  <+> fSale =: capital
            , specifics' <+> fCostCategory =: Fixed    <+> fSale =: fixed
            , specifics' <+> fCostCategory =: Variable <+> fSale =: variable
            ]
            ++
            [
              specifics' <+> fCostCategory =: cc (fMaterial <: rec) <+> fSale =: fSale <: rec
            |
              rec <- impacts
            , let cc = case fImpactCategory <: rec of
                         Consumption -> Direct
                         Production  -> Direct
                         Upstream    -> Indirect
            ]
        in
          (
                specifics'
            <+> fProduction =: (if isProduction (fProductive <: costs) then output else 0       )
            <+> fFlow       =: (if isProduction (fProductive <: costs) then 0        else output)
            <+> fLoss       =: 0
            <+> fSale       =: (sum $ (fSale <:) <$> cash)
          , cash
          , impacts
          )
      )
