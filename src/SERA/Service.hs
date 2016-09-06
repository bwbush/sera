-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for SERA.
--
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-orphans #-}


module SERA.Service (
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(String), typeMismatch)
import Data.Text (pack)
import Data.Void (Void)


instance FromJSON Void where
  parseJSON = typeMismatch "Void"

instance ToJSON Void where
  toJSON = String . pack . show
