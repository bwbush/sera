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
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Infrastructure.IO (
  InfrastructureFiles(..)
, readDemands
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import GHC.Generics (Generic)
import SERA.Infrastructure.Types (DemandCube)
import SERA.Types (FRegion, FYear)


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
