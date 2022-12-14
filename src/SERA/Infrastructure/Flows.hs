{-# LANGUAGE RecordWildCards #-}


module SERA.Infrastructure.Flows where


import Control.Arrow (second)
import Control.Monad (ap, guard)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import SERA.Types.Cubes (DemandCube, ExistingCube)
import SERA.Types.Fields (fCapacity, fFuelConsumption, Infrastructure, fInfrastructure, Location, fLocation, fNonFuelConsumption, Period(..), fPeriod, Year, fYear)


data TimeContext =
  TimeContext
  {
    yearz     :: [Year]
  , periods   :: [Period]
  , durations :: [Double]
  }
    deriving (Eq, Show)


newtype VaryingFlow = VaryingFlow { unvaryingFlow :: [(Double {- duration -}, Double {- rate -})] }
  deriving (Eq, Show)

instance Num VaryingFlow where
  VaryingFlow x + VaryingFlow y = VaryingFlow [(d, x' + y') | ((d, x'), (_, y')) <- zip x y]
  VaryingFlow x - VaryingFlow y = VaryingFlow [(d, x' - y') | ((d, x'), (_, y')) <- zip x y]
  _ * _ = error "VaryingFlows: cannot multiply flows."
  negate (VaryingFlow x) = VaryingFlow $ second negate <$> x
  abs    (VaryingFlow x) = VaryingFlow $ second abs    <$> x
  signum (VaryingFlow x) = VaryingFlow $ second signum <$> x
  fromInteger _ = error "VaryingFlows: cannot convert flow to integer."


(#*) :: VaryingFlow -> Double -> VaryingFlow
VaryingFlow x #* y = VaryingFlow $ second (* y) <$> x


peakFlow :: VaryingFlow -> Double
peakFlow (VaryingFlow x) = maximum $ abs . snd <$> x


sumFlow :: VaryingFlow -> Double
sumFlow (VaryingFlow x) = sum $ uncurry (*) <$> x


minimumOfVaryingFlow :: VaryingFlow -> Double
minimumOfVaryingFlow (VaryingFlow x) = minimum $ snd <$> x


allInfiniteVaryingFlow :: VaryingFlow -> Bool
allInfiniteVaryingFlow (VaryingFlow x) = all (isInfinite . snd) x


newtype VaryingFlows = VaryingFlows { unvaryingFlows :: [VaryingFlow] }
  deriving (Eq, Show)

instance Num VaryingFlows where
  VaryingFlows x + VaryingFlows y = VaryingFlows $ zipWith (+) x y
  VaryingFlows x - VaryingFlows y = VaryingFlows $ zipWith (-) x y
  VaryingFlows x * VaryingFlows y = VaryingFlows $ zipWith (*) x y
  negate      (VaryingFlows x) = VaryingFlows $ negate      <$> x
  abs         (VaryingFlows x) = VaryingFlows $ abs         <$> x
  signum      (VaryingFlows x) = VaryingFlows $ signum      <$> x
  fromInteger _ = error "VaryingFlowss: cannot convert flows to integer."


zeroFlows :: TimeContext -> VaryingFlows
zeroFlows = ap constantFlows $ flip replicate 0 . length . yearz


zeroFlows' :: VaryingFlows -> VaryingFlows
zeroFlows' (VaryingFlows dfs) = VaryingFlows $ VaryingFlow . (second (const 0) <$>) . unvaryingFlow <$> dfs


veryConstantFlows :: TimeContext -> Double -> VaryingFlows
veryConstantFlows tc@TimeContext{..} =  constantFlows tc . replicate (length yearz)


constantFlows :: TimeContext -> [Double] -> VaryingFlows
constantFlows TimeContext{..} = VaryingFlows . fmap (VaryingFlow . flip fmap durations . flip (,))


constantFlows' :: VaryingFlows -> [Double] -> VaryingFlows
constantFlows' (VaryingFlows dfs) = VaryingFlows . fmap (VaryingFlow . flip fmap (fst <$> unvaryingFlow (head dfs)) . flip (,)) -- FIXME


varyingFlows :: TimeContext -> DemandCube -> Location -> VaryingFlows
varyingFlows TimeContext{..} demandCube location =
  VaryingFlows
    [
      VaryingFlow [
        (
          duration
        , maybe 0 (\rec -> fFuelConsumption <: rec + fNonFuelConsumption <: rec)
          $ demandCube `evaluate` (fLocation =: location <+> fYear =: year <+> fPeriod =: period)
        )
      |
        (period, duration) <- zip periods durations
      ]
    |
      year <- yearz
    ]


varyingFlows':: TimeContext -> ExistingCube -> Infrastructure -> VaryingFlows
varyingFlows' TimeContext{..} existingCube infrastructure =
  let
    q = VaryingFlows
          [
            VaryingFlow [
              (
                duration
              , maybe 0 (\rec -> fCapacity <: rec)
                $ do
                   existing <- existingCube `evaluate` (fInfrastructure =: infrastructure <+> fPeriod =: period)
                   guard
                     $ fYear <: existing <= year
                   return existing
              )
            |
              (period, duration) <- zip periods durations
            ]
          |
            year <- yearz
          ]
  in
    if nonZeroFlows q
      then VaryingFlows
             [
               VaryingFlow
               [
                 (d, maximum [y, 0.001])
               |
                 let VaryingFlow ys = x
               , (d, y) <- ys
               ]
             |
               let VaryingFlows xs = q
             , x <- xs
             ]
      else q


totalFlow :: VaryingFlows -> Double
totalFlow = sum . totalFlows


totalFlows:: VaryingFlows -> [Double]
totalFlows(VaryingFlows dfs) = (sum . (uncurry (*) <$>)) . unvaryingFlow <$> dfs


peakAnnualFlows :: VaryingFlows -> [Double]
peakAnnualFlows (VaryingFlows df) = maximum . (snd <$>) . unvaryingFlow <$> df


minimumOfVaryingFlows :: VaryingFlows -> Double
minimumOfVaryingFlows (VaryingFlows x) = minimum $ minimumOfVaryingFlow <$> x


allInfiniteVaryingFlows :: VaryingFlows -> Bool
allInfiniteVaryingFlows (VaryingFlows x) = all allInfiniteVaryingFlow x


(.+.) :: VaryingFlows -> VaryingFlows -> VaryingFlows
VaryingFlows x .+. VaryingFlows y =
  VaryingFlows
    [
      VaryingFlow [
        (d, x'' + y'')
      |
        ((d, x''), (_, y'')) <- zip x' y'
      ]
    |
      (VaryingFlow x', VaryingFlow y') <- zip x y
    ]


(.-.) :: VaryingFlows -> VaryingFlows -> VaryingFlows
VaryingFlows x .-.VaryingFlows y =
  VaryingFlows
    [
      VaryingFlow [
        (d, x'' - y'')
      |
        ((d, x''), (_, y'')) <- zip x' y'
      ]
    |
      (VaryingFlow x', VaryingFlow y') <- zip x y
    ]


(#|<#) :: VaryingFlows -> VaryingFlows -> Bool
VaryingFlows x #|<# VaryingFlows y = or $ zipWith ((<) . abs) (snd <$> concat (unvaryingFlow <$> x)) (snd <$> concat (unvaryingFlow <$> y))


(#&<=#) :: VaryingFlows -> VaryingFlows -> Bool
VaryingFlows x #&<=# VaryingFlows y = and $ zipWith ((<=) . abs) (snd <$> concat (unvaryingFlow <$> x)) (snd <$> concat (unvaryingFlow <$> y))


(#<#) :: VaryingFlows -> VaryingFlows -> Bool
VaryingFlows x #<# VaryingFlows y =
  let
    x' = abs . snd <$> concat (unvaryingFlow <$> x)
    y' =       snd <$> concat (unvaryingFlow <$> y)
  in
    x' /= y' && and (zipWith (<=) x' y')


compatibleFlow :: VaryingFlows -> VaryingFlows -> Bool
VaryingFlows x `compatibleFlow` VaryingFlows y =
  let
    x' = snd <$> concat (unvaryingFlow <$> x)
    y' = snd <$> concat (unvaryingFlow <$> y)
  in
    and (zipWith (\x'' y'' -> x'' /= 0 || y'' == 0) x' y')


nonZeroFlows :: VaryingFlows -> Bool
nonZeroFlows (VaryingFlows xs) = all nonZeroFlow xs


nonZeroFlow :: VaryingFlow -> Bool
nonZeroFlow (VaryingFlow x) = all ((/= 0) . snd) x


storageRequirements :: VaryingFlows -> Double
storageRequirements (VaryingFlows xs) = maximum $ storageRequirement <$> xs


storageRequirement :: VaryingFlow -> Double
storageRequirement (VaryingFlow x) =
  let
    average = sum $ uncurry (*) <$> x
    x' = second (flip (-) average) <$> x
    cumulative = scanl1 (+) $ uncurry (*) <$> x'
  in
    maximum cumulative - minimum cumulative


levelizeCapacities :: VaryingFlows -> VaryingFlows
levelizeCapacities (VaryingFlows xs) = VaryingFlows $ levelizeCapacity <$> xs


levelizeCapacity :: VaryingFlow -> VaryingFlow
levelizeCapacity (VaryingFlow x) =
  let
    average = sum $ uncurry (*) <$> x
  in
    VaryingFlow $ second (const average) <$> x


utilization :: VaryingFlows -> Double
utilization (VaryingFlows xs) =
  let
    total = sum $ sumFlow <$> xs
    peak = maximum $ peakFlow <$> xs
  in
    abs $ total / peak


sumAbs :: VaryingFlows -> Double
sumAbs (VaryingFlows dfs) = sum $ abs . uncurry (*) <$> concat (unvaryingFlow <$> dfs)


maximumAbs :: VaryingFlows -> Double
maximumAbs (VaryingFlows dfs) = maximum $ abs . snd <$> concat (unvaryingFlow <$> dfs)


signumDiff :: VaryingFlows -> VaryingFlows -> [Double]
signumDiff (VaryingFlows x) (VaryingFlows y) =
  [
    foldl mostExtreme 0 [ -- FIXME: Check this.
      signum y'' * maximum [0, abs y'' - x'']
    |
      ((_, x''), (_, y'')) <- zip x' y'
    ]
  |
    (VaryingFlow x', VaryingFlow y') <- zip x y
  ]


absFlows :: VaryingFlows -> VaryingFlows
absFlows (VaryingFlows dfs) = VaryingFlows $ VaryingFlow . (second abs <$>) . unvaryingFlow <$> dfs


mostExtreme :: (Num a, Ord a) => a -> a -> a
mostExtreme x y =
  if abs x >= abs y
    then x
    else y


mostExtreme' :: Double -> VaryingFlows -> VaryingFlows
mostExtreme' x (VaryingFlows y) = VaryingFlows $ VaryingFlow . (second (mostExtreme x) <$>) . unvaryingFlow <$> y
