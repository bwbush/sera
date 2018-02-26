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
-- | Types for infrastructure networks.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric #-}


module SERA.Infrastructure (
-- * Types
  InfrastructureCubes(..)
, InfrastructureFiles(..)
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import SERA.Types.Cubes (CashCube, ConstructionCube, FlowCube, ImpactCube, SaleCube)


data InfrastructureCubes =
  InfrastructureCubes
  {
    constructionCube :: ConstructionCube
  , flowCube         :: FlowCube
  , cashCube         :: CashCube
  , impactCube       :: ImpactCube
  , saleCube         :: SaleCube
  }
    deriving (Eq, Ord, Show)


data InfrastructureFiles =
  InfrastructureFiles
  {
    constructionFile :: FilePath
  , flowFile         :: FilePath
  , cashFile         :: FilePath
  , impactFile       :: FilePath
  , saleFile         :: FilePath
  , geometryFile     :: FilePath
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON InfrastructureFiles

instance ToJSON InfrastructureFiles
