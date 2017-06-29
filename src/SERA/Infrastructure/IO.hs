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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Infrastructure.IO (
  InfrastructureFiles(..)
, readDemands
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Infrastructure.Types (DemandCube)


data InfrastructureFiles =
  InfrastructureFiles
  {
    constructionFile :: FilePath
  , flowFile         :: FilePath
  , cashFile         :: FilePath
  , impactFile       :: FilePath
  , saleFile         :: FilePath
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON InfrastructureFiles

instance ToJSON InfrastructureFiles


readDemands :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m DemandCube
readDemands = (mconcat <$>) . mapM readFieldCubeFile
