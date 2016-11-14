{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Demand {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  ProjectDemands
, unProjectDemands
, ProjectDemand(..)
, Demand(..)
, regionalizeDemands
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (nan)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Project (Project)
import SERA.Finance.Types (FcevIntroductions(..), FuelScenario(..))
import SERA.Util.Time (Year)


type ProjectDemands = [ProjectDemand]


unProjectDemands :: ProjectDemands -> [((Project, Year), Demand)]
unProjectDemands = map unProjectDemand


newtype ProjectDemand = ProjectDemand {unProjectDemand :: ((Project, Year), Demand)}
  deriving (Generic, Read, Show)

instance FromJSON ProjectDemand

instance ToJSON ProjectDemand where
  toJSON = genericToJSON defaultOptions

instance Tabulatable ProjectDemand where -- TODO: rewrite in pointfree style
  labels (ProjectDemand ((p, y), d)) = concat [labels p, labels y, labels d]
  tabulation (ProjectDemand ((p, y), d)) = concat[tabulation p, tabulation y, tabulation d]
  untabulation (pI : pN : y : d) = ProjectDemand ((untabulation [pI, pN], untabulation [y]), untabulation d)
  untabulation _ = undefined


data Demand =
  Demand
    {
      hydrogenDemand :: Double
    , fcevTotal      :: Double
    , fcevNew        :: Double
    , economyNet     :: Double
    , economyNew     :: Double
    , vmtTotal       :: Double
    }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON Demand

instance ToJSON Demand where
  toJSON = genericToJSON defaultOptions

instance Tabulatable Demand where
  labels _ =
    [
      "DEMAND"
    , "Year"
    , "Hydrogen Demand [kgH2 / day]"
    , "Total FCEVs [veh]"
    , "New FCEVs [veh]"
    , "Net FCEV Fuel Economy [mi / gge]"
    , "New FCEV Fuel Economy [mi / gge]"
    , "VMT [mi / veh]"
    ]
  tabulation Demand{..} =
    [
      ""
    , show hydrogenDemand
    , show fcevTotal
    , show fcevNew
    , show economyNet
    , show economyNew
    , show vmtTotal
    ]
  untabulation [hydrogenDemand', fcevTotal', fcevNew', economyNet', economyNew', vmtTotal'] =
    Demand
    {
      hydrogenDemand = read hydrogenDemand'
    , fcevTotal      = read fcevTotal'
    , fcevNew        = read fcevNew'
    , economyNet     = read economyNet'
    , economyNew     = read economyNew'
    , vmtTotal       = read vmtTotal'
    }
  untabulation _ = undefined


regionalizeDemands :: [(Project, Double)] -> [FcevIntroductions] -> [FuelScenario] -> ProjectDemands
regionalizeDemands proportions introductions demands =
  map ProjectDemand
    $ sort
    $ unProjectDemands 
    $ concat
      [
        [
          ProjectDemand (
            (p, y)
          , Demand
            {
              hydrogenDemand = f * demand
            , fcevTotal      = f * vehicles
            , fcevNew        = nan
            , economyNet     = nan
            , economyNew     = nan
            , vmtTotal       = nan
            }
          )
        |
          ((p, y), f) <- regionalizeDemands' proportions introductions year
        ]
      |
        (year, (demand, vehicles)) <- map unFuelScenario demands
      ]


regionalizeDemands' :: [(Project, Double)] -> [FcevIntroductions] -> Year -> [((Project, Year), Double)]
regionalizeDemands' proportions introductions year =
  let
    regions = map fst $ filter ((year >= ) . snd) $ map unFcevIntroductions introductions
    relevant = filter ((`elem` regions) . fst) proportions
    total = sum $ map snd relevant
  in
    [
      ((region, year), fromMaybe 0 (lookup region relevant) / total)
    |
      region <- regions
    ]
