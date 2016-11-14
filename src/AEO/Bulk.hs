{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards    #-}


module AEO.Bulk {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  AeoSeries(..)
, parseBulkAeo
, getBulkAeo
) where


import Codec.Compression.BZip (decompress)
import Control.Monad (when)
import Data.Aeson (FromJSON(parseJSON), Value(Number, Object, String), (.:), decode)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isSpace)
import Data.List (intercalate, sort)
import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)
import Data.Table (Tabulatable(..))
import Data.Text (unpack)

import Paths_sera (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as B


data AeoSeries =
  AeoSeries
    {
      aeoId                   :: String
    , aeoName                 :: [String]
    , aeoUnits                :: String
    , aeoUnitsShort           :: String
    , aeoDescription          :: String
    , aeoStart                :: Int
    , aeoEnd                  :: Int
    , aeoLastHistoricalPeriod :: Int
    , aeoLastUpdated          :: String  -- FIXME: Use an ISO date here.
    , aeoData                 :: [(Int, Double)]
    }
    deriving (Show)

instance FromJSON AeoSeries where
  parseJSON (Object x) =
    AeoSeries
      <$> x .: "series_id"
      <*> (map strip . splitOneOf ":," <$> x .: "name")
      <*> x .: "units"
      <*> x .: "unitsshort"
      <*> x .: "description"
      <*> (read <$> x .: "start")
      <*> (read <$> x .: "end")
      <*> (read <$> x .: "lastHistoricalPeriod")
      <*> x .: "last_updated"
      <*> (sort . map unpackData <$> x .: "data")
        where
          unpackData :: [Value] -> (Int, Double)
          unpackData [String y, Number v] = (read $ unpack y, realToFrac v) -- FIXME: Simplify this.
          unpackData _ = error "Expected an Array"
  parseJSON _ = fail "Expected an Object"

instance Tabulatable AeoSeries where
  labels _ =
    [
      "Series ID"
    , "Name"
    , "Units"
    , "Short Units"
    , "Description"
    , "Start"
    , "End"
    , "Last Historical Period"
    ]
  tabulation AeoSeries{..} =
    [
      aeoId
    , intercalate " » " aeoName
    , aeoUnits
    , aeoUnitsShort
    , aeoDescription
    , show aeoStart
    , show aeoEnd
    , show aeoLastHistoricalPeriod
    , aeoLastUpdated
    ]
  untabulation = undefined


parseBulkAeo :: ByteString -> Maybe AeoSeries
parseBulkAeo = decode
    

getBulkAeo :: Int -> IO [AeoSeries]
getBulkAeo year' =
  do
    checkYear year'
    bulkFile <- getDataFileName "aeo/2014/AEO.txt.bz2"
    mapMaybe parseBulkAeo . B.lines . decompress <$> B.readFile bulkFile


checkYear :: Monad m => Int -> m ()
checkYear year' = when (year' /= 2014) (error $ "Publication year " ++ show year' ++ " not available.")


strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
