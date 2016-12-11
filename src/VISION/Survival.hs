-----------------------------------------------------------------------------
--
-- Module      :  VISION.Survival
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Vehicle survival from VISION 2016.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}


module VISION.Survival (
-- * Types
  SurvivalData(..)
-- * Functions
, survival
, survivalLDV
, survivalHDV
) where


import Data.Aeson (FromJSON, ToJSON)
import Control.Monad (guard)
import Data.Daft.DataCube.Function (fromFunction)
import Data.Daft.Vinyl.FieldCube (φ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Default (Default(..))
import Data.List (inits)
import GHC.Generics (Generic)
import SERA.Vehicle.Types (fAge, fSurvival)
import SERA.Vehicle.Stock.Types (SurvivalCube)


data SurvivalData =
  SurvivalData
    {
      a :: Double
    , b :: Double
    , c :: Double
    , d :: Double
    }
      deriving (Generic, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def =
    SurvivalData
    {
      a = 1.649
    , b = -0.121
    , c = 3.381
    , d = -0.286
    }


survivalLDV :: SurvivalCube
survivalLDV = survival def


survival :: SurvivalData -> SurvivalCube
survival survivalData =
  φ . fromFunction $ \rec ->
    do
      let
        products =
          map product
            $ inits
              [factor survivalData $ fromIntegral a | a <- [(0::Int)..]]
        age = fAge <: rec
      guard
        $ age >= 0
      return
        $ fSurvival =: products !! age


factor :: SurvivalData -> Double -> Double
factor SurvivalData{..} age
  | age == 0  = 1
  | age <= 10 = (1 - exp(- exp(a + b * age))) / (1 - exp(- exp(a + b * (age - 1))))
  | age == 11 = (1 - exp(- exp(c + d * age))) / (1 - exp(- exp(a + b * (age - 1))))
  | age <= 20 = (1 - exp(- exp(c + d * age))) / (1 - exp(- exp(c + d * (age - 1))))
  | otherwise =
      let
        x = [1 - exp(-exp(c + d * i)) | i <- [21..27]]
      in
        sum x / fromIntegral (length x) / (1 - exp(- exp(c + d * 20)))


-- | Survival function for medium and heavy duty vehicles from VISION 2016.
survivalHDV :: SurvivalCube
survivalHDV =
  φ . fromFunction $ \rec ->
    do
      let
        products =
          map product
            . inits
            $  replicate 8 0.99
            ++ replicate 2 0.97
            ++ replicate 3 0.93
            ++ replicate 1 0.90
            ++ replicate 5 0.86
            ++ repeat      0.82
        age = fAge <: rec
      guard
        $ age >= 0
      return
        $ fSurvival =: products !! age
