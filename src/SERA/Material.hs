-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Materials and prices.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material (
  Pricer
, makePricer
, materials
, upstreamMaterials
, readPrices
, readIntensities
, checkPrices
, checkIntensities
, rezonePrices
, rezoneIntensities
, localize
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logError, logInfo, logNotice, logWarning)
import Data.Daft.DataCube (DataCube(Key, Keys))
import Data.Daft.Vinyl.FieldCube (FieldCube, (⋈), κ, σ, ω, υ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import Data.Vinyl.TypeLevel (type (++))
import SERA (SeraLog, checkPresent, readConcat)
import SERA.Network (Network(..))
import SERA.Process (ProcessLibrary(..))
import SERA.Types.Cubes (IntensityCube, PriceCube, ZoneCube)
import SERA.Types.Fields (fBillable, fFraction, fIntensity, Location, FLocation, fLocation, Material, FMaterial, fMaterial, fPrice, FQuantity, fQuantity, FUpstreamMaterial, fUpstreamMaterial, Year, FYear, fYear, FZone, fZone)
import SERA.Util (extractKey)

import qualified Data.Map as M (lookupGE)
import qualified Data.Set as S (union, unions)


makePricer :: Ord (FieldRec (key ++ '[FMaterial, FYear, FQuantity])) => PriceCube key -> FieldRec key -> Double -> Pricer
makePricer priceCube key quantity material year =
  maybe 0 ((fPrice <:) . snd)
    . flip M.lookupGE priceCube
    $ key <+> (fMaterial =: material <+> fYear =: year <+> fQuantity =: quantity)


materials :: (FMaterial ∈ ks, Key cube Material, DataCube cube, Keys cube ~ Set) => FieldCube cube ks vs -> Set Material
materials = υ (fMaterial <:)


upstreamMaterials :: (FUpstreamMaterial ∈ ks, Key cube Material, DataCube cube, Keys cube ~ Set) => FieldCube cube ks vs -> Set Material
upstreamMaterials = υ (fUpstreamMaterial <:)


type Pricer = Material -> Year -> Double


readPrices :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (PriceCube '[FZone])
readPrices = readConcat "prices" "price key"


checkPrices :: SeraLog m => Network -> ProcessLibrary -> IntensityCube '[FZone] -> PriceCube '[FZone] -> m ()
checkPrices Network{..} ProcessLibrary{..} intensityCube priceCube =
  do
    logInfo "Checking prices . . ."
    let
      priceMaterials      = extractKey (fMaterial <:)         priceCube
      inputMaterials      = extractKey (fMaterial <:)         processInputCube
      outputMaterials     = extractKey (fMaterial <:)         processOutputCube
      intensityMaterials  = extractKey (fMaterial <:)         intensityCube
      intensityMaterials' = extractKey (fUpstreamMaterial <:) intensityCube
    -- FIXME: add more tests.
    checkPresent
      logError
      "Prices"
      (extractKey (fZone <:) priceCube)
      "network zones"
      (extractKey (fZone <:) zoneCube)
    checkPresent
      logWarning
      "Process inputs"
      inputMaterials
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Process outputs"
      outputMaterials
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Upstream emission intensities"
      (intensityMaterials `S.union` intensityMaterials')
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Prices"
      priceMaterials
      "process inputs, process outputs, or upstream emissions intensities"
      $ S.unions [inputMaterials, outputMaterials, intensityMaterials, intensityMaterials']


readIntensities :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (IntensityCube '[FZone])
readIntensities = readConcat "upstream emission intensities" "intensity key"


checkIntensities :: SeraLog m => Network -> ProcessLibrary -> IntensityCube '[FZone] -> m ()
checkIntensities Network{..} ProcessLibrary{..} intensityCube =
  do
    logInfo "Checking upstream emission intensities . . ."
    let
      inputMaterials      = extractKey (fMaterial <:)         processInputCube
      outputMaterials     = extractKey (fMaterial <:)         processOutputCube
      intensityMaterials  = extractKey (fMaterial <:)         intensityCube
    checkPresent
      logError
      "Upstream emission intensities"
      (extractKey (fZone <:) intensityCube)
      "network zones"
      (extractKey (fZone <:) zoneCube)
    checkPresent
      logWarning
      "Upstream emissions intensities"
      intensityMaterials
      "process inputs or process outputs"
      (inputMaterials `S.union` outputMaterials)


rezonePrices :: PriceCube '[FZone]
             -> ZoneCube '[FLocation]
             -> PriceCube '[FLocation]
rezonePrices prices zones = -- FIXME: Generalize this to `key` instead of `FLocation`.
  κ (ω zones :: Set (FieldRec '[FZone])) combine
    $ prices ⋈ zones
    where
      combine _ fps =
            fPrice    =: sum [fFraction <: fp * fPrice <: fp | fp <- fps]
        <+> fBillable =: or  [fBillable <: fp                | fp <- fps]  -- FIXME: Generalize.


rezoneIntensities :: IntensityCube '[FZone]
             -> ZoneCube '[FLocation]
             -> IntensityCube '[FLocation]
rezoneIntensities intensities zones = -- FIXME: Generalize this to `key` instead of `FLocation`.
  κ (ω zones :: Set (FieldRec '[FZone])) combine
    $ intensities ⋈ zones
    where
      combine _ fps =
            fIntensity    =: sum [fFraction <: fp * fIntensity <: fp | fp <- fps]


localize :: IntensityCube '[FLocation] -> Location -> IntensityCube '[]
localize intensities location =
  κ (undefined :: Set (FieldRec '[FLocation])) (\_ recs -> head recs)
    $ σ (\key _ -> location == fLocation <: key) intensities

