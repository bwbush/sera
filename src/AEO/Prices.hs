module AEO.Prices {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Price
, PriceKey
, AeoPrices(..)
, Pricer
, makePricer
, readPrices
, writePrices
, lookupAEO
) where


import AEO.Types (Region(..), Sector(..), Source(..))
import Control.Arrow (second)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import SERA.Util.Units (CurrencyUnits(..), EnergyUnits(..))
import SERA.Util.Time (Year(..))
import System.IO.Unsafe (unsafePerformIO)

import Paths_sera (getDataFileName)

import qualified Data.Map as M


-- | Prices.
type Price = Double


-- | Key for looking up prices.
type PriceKey = (Region, Sector, Source, Year)


-- | AEO price data.
data AeoPrices =
  AeoPrices {
    aeoDateKey :: Int                  -- ^ Public release date.
  , aeoCase    :: String               -- ^ Case name.
  , aeoUnits   :: String               -- ^ Unit of measure.
  , aeoPrices  :: M.Map PriceKey Price -- ^ Price data.
  }
    deriving (Read, Show)


-- | Function for looking up energy prices.
type Pricer =
     Region -- ^ Geographic region.
  -> Sector -- ^ Usage sector.
  -> Source -- ^ Energy source.
  -> Year   -- ^ Year.
  -> Price  -- ^ Energy price.


-- | Make a function for looking up energy prices from AEO data.
makePricer ::
     AeoPrices     -- ^ Price data.
  -> CurrencyUnits -- ^ Unit of measure for currency.
  -> EnergyUnits   -- ^ Unit of measure for energy.
  -> Pricer        -- ^ Function for looking up energy prices.
makePricer prices Dollars MillionBtu region sector source year = fromMaybe (error $ "Energy price for "++ show (region, sector, source, year) ++ " not found.") $ M.lookup (region, sector, source, year) $ aeoPrices prices
makePricer prices currency Kwh region sector source year = 0.0034095106405145 * makePricer prices currency MillionBtu region sector source year
makePricer prices currency Gge region sector source year = 0.114 * makePricer prices currency MillionBtu region sector source year


readPrices :: FilePath -> IO AeoPrices
readPrices inFile =
  do
    let
      parsePrice :: [String] -> (PriceKey, Price)
      parsePrice [reg, sec, sou, yea, pri] = ((read reg, read sec, read sou, Year $ read yea), read pri)
      parsePrice _ = undefined
    prices <- readFile inFile
    return
      AeoPrices
        {
          aeoDateKey     = 0
        , aeoCase        = ""
        , aeoUnits       = ""
        , aeoPrices      = M.fromList $ map (parsePrice . splitOn "\t") $ tail $ lines prices
        }


-- | Dump price data.
writePrices ::
     AeoPrices -- ^ Price data.
  -> [String]  -- ^ Tab-separated values for the price data.
writePrices prices =
  "Region\tSector\tSource\tYear\tPrice [nom$/mmBTU]"
    : map (\((region, sector, source, year), price) -> intercalate "\t" [show region, show sector, show source, show $ unYear year, show price]) (M.toList $ aeoPrices prices)


aeoData :: [(String, AeoPrices)]
aeoData =
  map (second (unsafePerformIO . (readPrices =<<) . getDataFileName))
    [
      ("AEO_2014_REF", "aeo/2014/REF2014/PRCE_NOM.tsv")
    , ("AEO_2014_LP" , "aeo/2014/LOWPRICE/PRCE_NOM.tsv")
    , ("AEO_2014_HP" , "aeo/2014/HIGHPRICE/PRCE_NOM.tsv")
    ]


lookupAEO :: CurrencyUnits -> EnergyUnits -> String -> Region -> Sector -> Source -> Int -> Double
lookupAEO currency energy scenario region sector source year =
  let
    series =
      fromMaybe (error $ "AEO case \"" ++ scenario ++ "\" in year " ++ show year ++ " not found.")
        $ lookup scenario aeoData
  in
    makePricer series currency energy region sector source (Year year)
