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


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module VISION.Survival (
-- * Functions
  survivalMHD
) where


import Control.Monad (guard)
import Data.Daft.DataCube (fromFunction)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.List (inits)
import SERA.Vehicle.Types (fAge, fSurvival)
import SERA.Vehicle.Stock.Types (SurvivalCube)


-- | Survival function for medium and heavy duty vehicles from VISION 2016.
survivalMHD :: SurvivalCube
survivalMHD =
  fromFunction $ \rec ->
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
