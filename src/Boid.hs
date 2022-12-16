module Boid (Boid, newBoid, updateBoid, bPos, bVel) where

import Config (Config (..), WorldSize (..))
import Control.DeepSeq (NFData (..))
import Data.List (sortOn)
import Linear (negated)
import Linear.Metric (Metric (norm))
import Linear.V2 (V2 (..))
import Linear.Vector (zero, (*^), (^+^), (^-^), (^/))
import Utils (vBound, vWrap, wrapDisp)

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

between :: Config -> Boid -> Boid -> V2 Float
between cfg b bo = case wSize cfg of
  Size size -> wrapDisp size (bPos b) (bPos bo)
  Infinite -> bPos bo ^-^ bPos b

flockmates :: Config -> [Boid] -> Float -> Boid -> [(Boid, V2 Float)]
flockmates cfg flock r b = filter (\(bo, _) -> bPos bo /= bPos b) neighbors
  where
    neighbors = takeWhile (\(_, disp) -> norm disp < r) sorted
    sorted = sortOn (norm . snd) $ map (\bo -> (bo, between cfg b bo)) flock

wrapPos :: Config -> V2 Float -> V2 Float
wrapPos cfg pos = case wSize cfg of
  Size size -> vWrap size pos
  Infinite -> pos

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
steerFrom :: Boid -> Steer -> (Boid, V2 Float) -> Steer
steerFrom b (Steer sf af cf) disp = Steer sf' af' cf'
  where
    sf' = sf ^+^ separation b disp
    af' = af ^+^ alignment b disp
    cf' = cf ^+^ cohesion b disp

-- update boid with all flockmates
updateBoid :: Config -> [Boid] -> Boid -> Boid
updateBoid cfg flock b = b {bPos = pos', bVel = vel'}
  where
    pos' = wrapPos cfg $ bPos b ^+^ 0.1 *^ vel'
    vel' = vBound (maxVel cfg) (bVel b ^+^ 0.05 *^ netf ^/ bMass b)
    netf = sn cfg *^ sf ^+^ an cfg *^ af ^+^ cn cfg *^ cf
    Steer sf af cf = foldl (steerFrom b) initSteer bs
    bs = flockmates cfg flock (radius cfg) b

--------------------------------------------------------------------------------

separation :: Boid -> (Boid, V2 Float) -> V2 Float
separation _ (_, disp) = negated disp ^/ (norm disp ** 2)

alignment :: Boid -> (Boid, V2 Float) -> V2 Float
alignment b (bo, _) = bVel bo ^-^ bVel b

cohesion :: Boid -> (Boid, V2 Float) -> V2 Float
cohesion _ (_, disp) = disp