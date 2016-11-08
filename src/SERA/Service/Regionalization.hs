-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Regionalization
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for estimating introduction years.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.Regionalization (
-- * Configuration
  ConfigRegionalization(..)
-- * Computation
, calculateRegionalization
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Scenario.Regionalization (regionalize)


-- | Configuration for vehicle stock modeling.
data ConfigRegionalization =
  ConfigRegionalization
  {
    regionalIntroductionsSource :: DataSource Void        -- ^ Outputs.
  , totalStockSource            :: DataSource Void
  , regionalStockSource         :: DataSource Void
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigRegionalization

instance ToJSON ConfigRegionalization


-- | Compute introduction years.
calculateRegionalization :: (IsString e, MonadError e m, MonadIO m)
                         => ConfigRegionalization -- ^ Configuration data.
                         -> m ()                  -- ^ Action to compute the introduction years.
calculateRegionalization ConfigRegionalization{..} =
  do
    inform $ "Reading regional introduction years from " ++ show regionalIntroductionsSource ++ " . . ."
    regionalIntroductions <- readFieldCubeSource regionalIntroductionsSource
    inform $ "Reading total stock from " ++ show totalStockSource ++ " . . ."
    totalStock <- readFieldCubeSource totalStockSource
    let
      regionalStock = regionalize regionalIntroductions totalStock
    withSource regionalStockSource $ \source -> do
      inform $ "Writing regional stock to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source regionalStock
