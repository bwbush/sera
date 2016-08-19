{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}


module SERA.Vehicle.Stock.Travel (
  AnnualTravelModel(..)
, annualTravelFunction
) where


import Control.Arrow (second)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Lookup (asLookupTable, lookupOrd, lookupReal)
import Data.Daft.Vinyl.FieldRec ((<:), readFieldRecFile)
import Data.List.Util (sortedGroups)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Types (fRegion)
import SERA.Vehicle.Types (fAge, fAnnualTravel, fClassification, fFuel)
import SERA.Vehicle.Stock.Types (AnnualTravelFunction, AnnualTravelRecord)

import qualified SERA.Vehicle.MHD.Census2002 as Census2002
import qualified SERA.Vehicle.MHD.EMFAC2010 as EMFAC2010
import qualified SERA.Vehicle.MHD.Mitchell20141106 as Mitchell20141106


data AnnualTravelModel =
    Census2002
  | EMFAC2010
  | Mitchell20141106
  | AnnualTravelFile FilePath
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON AnnualTravelModel where

instance ToJSON AnnualTravelModel where


annualTravelFunction :: (IsString e, MonadError e m, MonadIO m) => AnnualTravelModel -> m AnnualTravelFunction
annualTravelFunction Census2002                  = return Census2002.annualTravelFunction
annualTravelFunction EMFAC2010                   = return EMFAC2010.annualTravelFunction
annualTravelFunction Mitchell20141106            = return Mitchell20141106.annualTravelFunction
annualTravelFunction (AnnualTravelFile filePath) = annualTravelTabulation filePath


annualTravelTabulation :: (IsString e, MonadError e m, MonadIO m) => FilePath -> m AnnualTravelFunction
annualTravelTabulation filePath =
  do
    raw <- readFieldRecFile filePath
    let
      table =
        asLookupTable
        . fmap (second asLookupTable)
        $ sortedGroups
        [
          (
            (
              fRegion         <: record
            , fClassification <: record
            , fFuel           <: record    
            )
          , (
              fAge            <: record
            , fAnnualTravel   <: record
            )
          )
        |
          record <- raw :: [AnnualTravelRecord]
        ]
    return $ \region classification age fuel ->
      age `lookupReal` ((region, classification, fuel) `lookupOrd` table)
