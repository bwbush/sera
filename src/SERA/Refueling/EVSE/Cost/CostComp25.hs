-- | EVSE cost data from the spreadsheet "Infrastructure Cost Comparison v23.xlsx".
module SERA.Refueling.EVSE.Cost.CostComp25 {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Scenario
, scenarios
, chargers
, cost
) where


import Data.Maybe (fromJust)
import Data.Tuple.Util (fst3)
import SERA.Refueling.EVSE.Cost (Charger(..), Coster, CostData, makeCoster)
import SERA.Util.Time (Year(..))


-- | List of chargers.
chargers :: [Charger]
chargers = map (Charger . fst3) $ snd $ head costData


-- | A scenario name.
type Scenario = String


-- | List of scenarios.
scenarios :: [Scenario]
scenarios = map fst costData


-- | Function for computing charger cost for a particular scenario.
cost ::
     Scenario -- ^ The scenario.
  -> Coster   -- ^ The cost function.
cost scenario charger  _ = cost' scenario charger (Year 2025)


-- | Function for computing charger cost for a particular scenario.
cost' ::
     Scenario -- ^ The scenario.
  -> Coster   -- ^ The cost function.
cost' = makeCoster . fromJust . flip lookup costData


-- | The cost data.
costData :: [(Scenario, CostData)]
costData =
  [
    (
      scenario
    , [
        (
          charger
        , 2025
        , fromJust (equipmentCost (Charger charger) $ Year 2025) + fromJust (installationCost (Charger charger) $ Year 2025))
      | charger <- map fst3 $ snd $ head equipmentCostData
      ]
    )
  | scenario <- map fst equipmentCostData
  , let equipmentCost = makeCoster $ fromJust $ lookup scenario equipmentCostData 
  , let installationCost = makeCoster $ fromJust $ lookup scenario installationCostData
  ]


-- | The equipment cost data.
equipmentCostData :: [(Scenario, CostData)]
equipmentCostData =
  [
    (
      "Low Station Costs"
    , [
        ("L1"     , 2025,   331.65)
      , ("L2 Home", 2025,   771.22)
      , ("L2 Work", 2025,  2477.87)
      , ("DC Fast", 2025, 20000/2 )
      ]
    )
  , (
      "Medium Station Costs"
    , [
        ("L1"     , 2025,   495.00)
      , ("L2 Home", 2025,   924.50)
      , ("L2 Work", 2025,  2861.00)
      , ("DC Fast", 2025, 54990/2 )
      ]
    )
  , (
      "High Station Costs"
    , [
        ("L1"     , 2025,   582.45)
      , ("L2 Home", 2025,  1534.18)
      , ("L2 Work", 2025,  3401.87)
      , ("DC Fast", 2025, 80000/2 )
      ]
    )
  ]


-- | The installation cost data.
installationCostData :: [(Scenario, CostData)]
installationCostData =
  [
    (
      "Low Station Costs"
    , [
        ("L1"     , 2025,   330.09)
      , ("L2 Home", 2025,   988.78)
      , ("L2 Work", 2025,  2977.44)
      , ("DC Fast", 2025, 20000/2 )
      ]
    )
  , (
      "Medium Station Costs"
    , [
        ("L1"     , 2025,   492.67)
      , ("L2 Home", 2025,  1320.15)
      , ("L2 Work", 2025,  3457.00)
      , ("DC Fast", 2025, 54990/2 )
      ]
    )
  , (
      "High Station Costs"
    , [
        ("L1"     , 2025,   537.33)
      , ("L2 Home", 2025,  2260.02)
      , ("L2 Work", 2025,  3913.72)
      , ("DC Fast", 2025, 80000/2 )
      ]
    )
  ]
