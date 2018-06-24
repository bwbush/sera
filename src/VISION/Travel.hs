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


module VISION.Travel (
-- * Types
  TravelData
-- * Functions
, travel
, travelLDV
) where


import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Daft.DataCube.Function (fromFunction)
import Data.Daft.Vinyl.FieldCube (ε)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Default (Default(..))
import GHC.Generics (Generic)
import SERA.Types.Fields (fAge, fAnnualTravel)
import SERA.Types.Cubes (AnnualTravelCube)


data TravelData =
  TravelData
    {
      vmt0          :: Double
    , dr            :: Double
    , dv            :: Double
    , v0            :: Double
    }
      deriving (Generic, Read, Show)

instance FromJSON TravelData

instance ToJSON TravelData where

instance Default TravelData where
  def =
    TravelData
    {
      vmt0          = 14476.0
    , dr            = -232.8
    , dv            = -13.2
    , v0            = 0.367
    }


travelLDV :: AnnualTravelCube
travelLDV = travel def


travel :: TravelData -> AnnualTravelCube
travel TravelData{..} =
  ε . fromFunction $ \rec ->
    do
      let
        age = fromIntegral $ fAge <: rec
        f :: Double -> Double
        f a = vmt0 + dr * a + dv * a^(2 :: Int) + v0 * a^(3 :: Int)
        x = [f i | i <- [21..27]]
        xave = sum x / fromIntegral (length x)
      guard
        $ age >= 0
      return
        $ fAnnualTravel =: if age <= 20 then f age else xave
