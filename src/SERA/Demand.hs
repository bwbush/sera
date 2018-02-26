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
-- | Energy demand.
--
-----------------------------------------------------------------------------


module SERA.Demand (
-- * Input/output
  readDemands
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import SERA.Types.Cubes (DemandCube)


readDemands :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m DemandCube
readDemands = (mconcat <$>) . mapM readFieldCubeFile
