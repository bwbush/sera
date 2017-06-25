-----------------------------------------------------------------------------
--
-- Module      :  SERA.Energy.Prices
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Energy prices.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Network.IO (
  readZones
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import SERA.Network.Types (FLocation, FZone, ZoneCube)
import SERA.Types (FRegion, FYear)


readZones :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (ZoneCube '[FLocation])
readZones = (mconcat <$>) . mapM readFieldCubeFile
