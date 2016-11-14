-- | EVSE cost data from the spreadsheet "Infrastructure Cost Comparison v23.xlsx".
module SERA.Refueling.EVSE.Cost.CostComp23 {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
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
        ("L1"     , 2025,   301.50)
      , ("L2 Home", 2025,   754.47)
      , ("L2 Work", 2025,  2537.50)
      , ("DC Fast", 2025, 10000   )
      ]
    )
  , (
      "Medium Station Costs"
    , [
        ("L1"     , 2025,   450   )
      , ("L2 Home", 2025,   899.50)
      , ("L2 Work", 2025,  2950.00)
      , ("DC Fast", 2025, 27500   )
      ]
    )
  , (
      "High Station Costs"
    , [
        ("L1"     , 2025,   552.30)
      , ("L2 Home", 2025,  1517.43)
      , ("L2 Work", 2025,  3461.50)
      , ("DC Fast", 2025, 60000   )
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
      , ("L2 Home", 2025,  1011.22)
      , ("L2 Work", 2025,  3431.84)
      , ("DC Fast", 2025, 10000   )
      ]
    )
  , (
      "Medium Station Costs"
    , [
        ("L1"     , 2025,   492.67)
      , ("L2 Home", 2025,  1320.15)
      , ("L2 Work", 2025,  4135.20)
      , ("DC Fast", 2025, 15000   )
      ]
    )
  , (
      "High Station Costs"
    , [
        ("L1"     , 2025,   537.33)
      , ("L2 Home", 2025,  2709.72)
      , ("L2 Work", 2025,  4481.30)
      , ("DC Fast", 2025, 20000   )
      ]
    )
  ]
