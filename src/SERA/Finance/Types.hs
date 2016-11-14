module SERA.Finance.Types {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  FcevIntroductions(..)
, FuelScenario(..)
, StationSizing(..)
) where


import Control.Arrow (first, second)
import Data.Table (Tabulatable(..), labels1, labels3, tabulation1, tabulation3)
import Data.Table.Identifier (Id(..))
import Data.Tuple.Util (uncurry3)
import SERA.Constants (days_per_year, gge_per_J, kgH2_per_J)
import SERA.Finance.Project (Project)
import SERA.Util.Time (Year(..))


newtype FcevIntroductions = FcevIntroductions {unFcevIntroductions :: (Project, Year)}
  deriving (Read, Show)

instance Tabulatable FcevIntroductions where
  labels = labels1 "FCEV Introduction [Year]"
  tabulation = uncurry tabulation1 . unFcevIntroductions
  untabulation [pI, pN, x] = FcevIntroductions (untabulation [pI, pN], Year $ read x)
  untabulation _ = undefined


newtype FuelScenario = FuelScenario {unFuelScenario :: (Year, (Double, Double))}
  deriving (Read, Show)

instance Tabulatable FuelScenario where
  labels = labels1 "Fuel Use [B gge/yr]" . fst . unFuelScenario
  tabulation = uncurry tabulation1 . second (first (1e-9 / kgH2_per_J * gge_per_J * days_per_year *) . first (1e-6 *)) . unFuelScenario
  untabulation [y, x, v, _, _] = FuelScenario (untabulation [y], (1e9 * kgH2_per_J / gge_per_J / days_per_year * read x, 1e6 * read v))
  untabulation _ = undefined


newtype StationSizing = StationSizing {unStationSizing :: ((Project, Year, Id Int), Double)}
  deriving (Read, Show)

instance Tabulatable StationSizing where
  labels = uncurry3 (labels3 "Capacity [kg/day]") . fst . unStationSizing
  tabulation = uncurry (uncurry3 tabulation3) . unStationSizing
  untabulation [uaI, uaN, y, i, x] = StationSizing ((untabulation [uaI, uaN], untabulation [y], untabulation [i]), read x)
  untabulation _ = undefined
