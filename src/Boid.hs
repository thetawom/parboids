module Boid (Boid, newBoid, updateBoid, bPos, bVel) where

import Config (an, cn, maxVel, radius, sn)
import Control.DeepSeq (NFData (..))
import Data.List (sortOn)
import Linear.Metric (Metric (distance))
import Linear.V2 (V2 (..))
import Linear.Vector (zero, (*^), (^+^), (^-^), (^/))
import Utils (vBound)

--------------------------------------------------------------------------------

data Boid = Boid
  { bPos :: V2 Float,
    bVel :: V2 Float,
    bMass :: Float
  }

instance Show Boid where
  show (Boid (V2 px py) (V2 vx vy) m) = show px ++ " " ++ show py ++ " " ++ show vx ++ " " ++ show vy ++ " " ++ show m

instance NFData Boid where
  rnf (Boid pos vel m) = rnf pos `seq` rnf vel `seq` rnf m

--------------------------------------------------------------------------------

newBoid :: [Float] -> Maybe Boid
newBoid [px, py, vx, vy, m] = Just $ Boid (V2 px py) (V2 vx vy) m
newBoid [px, py, vx, vy] = Just $ Boid (V2 px py) (V2 vx vy) 1
newBoid _ = Nothing

dist :: Boid -> Boid -> Float
dist b bo = distance (bPos b) (bPos bo)

flockmates :: [Boid] -> Float -> Boid -> [(Boid, Float)]
flockmates flock r b = filter (\(bo, _) -> bPos bo /= bPos b) neighbors
  where
    neighbors = takeWhile (\(_, d) -> d < r) sorted
    sorted = sortOn snd $ map (\bo -> (bo, dist b bo)) flock

--------------------------------------------------------------------------------

data Steer = Steer
  { sSf :: V2 Float,
    sAf :: V2 Float,
    sCf :: V2 Float
  }
  deriving (Show)

-- initialize steering force
initSteer :: Steer
initSteer = Steer zero zero zero

-- add steering force from another boid
steerFrom :: Boid -> Steer -> (Boid, Float) -> Steer
steerFrom b (Steer sf af cf) bod = Steer sf' af' cf'
  where
    sf' = sf ^+^ separation b bod
    af' = af ^+^ alignment b bod
    cf' = cf ^+^ cohesion b bod

-- update boid with all flockmates
updateBoid :: [Boid] -> Boid -> Boid
updateBoid flock b@(Boid pos vel m) = Boid {bPos = pos', bVel = vel', bMass = m}
  where
    pos' = pos ^+^ 0.5 *^ vel'
    vel' = vBound maxVel (vel ^+^ 0.05 *^ netf ^/ m)
    netf = sn *^ sf ^+^ an *^ af ^+^ cn *^ cf
    Steer sf af cf = foldl (steerFrom b) initSteer bs
    bs = flockmates flock radius b

--------------------------------------------------------------------------------

separation :: Boid -> (Boid, Float) -> V2 Float
separation b (bo, d) = (bPos b ^-^ bPos bo) ^/ (d ** 2)

alignment :: Boid -> (Boid, Float) -> V2 Float
alignment b (bo, _) = bVel bo ^-^ bVel b

cohesion :: Boid -> (Boid, Float) -> V2 Float
cohesion b (bo, _) = bPos bo ^-^ bPos b