module Sim (runSim, runSimCollect) where

import Boid (Boid, updateBoid)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (parList, parListChunk, rdeepseq, rpar, rseq, runEval, using)

--------------------------------------------------------------------------------

data ParStrat = Seq | TwoPart | Chunks Int | ParList

updateWith :: ParStrat -> [Boid] -> [Boid]
updateWith Seq = updateSeq
updateWith TwoPart = updateTwoPart
updateWith (Chunks n) = updateChunks n
updateWith ParList = updateParList

--------------------------------------------------------------------------------

runSim :: [Boid] -> Int -> [Boid]
runSim flock0 nIter = case runSimCollect flock0 nIter of
  [] -> flock0
  (flockN : _) -> flockN

runSimCollect :: [Boid] -> Int -> [[Boid]]
runSimCollect flock0 nIter = foldl simLoop [flock0] [1 .. nIter]
  where
    simLoop :: [[Boid]] -> Int -> [[Boid]]
    simLoop [] _ = []
    simLoop flocks@(flock : _) _ = updateWith Seq flock : flocks

--------------------------------------------------------------------------------

updateSeq :: [Boid] -> [Boid]
updateSeq flock = map (updateBoid flock) flock

updateTwoPart :: [Boid] -> [Boid]
updateTwoPart flock = runEval $ do
  as' <- rpar (force (map (updateBoid flock) as))
  bs' <- rpar (force (map (updateBoid flock) bs))
  _ <- rseq as'
  _ <- rseq bs'
  return (as' ++ bs')
  where
    (as, bs) = splitAt (length flock `div` 2) flock

updateChunks :: Int -> [Boid] -> [Boid]
updateChunks numChunks flock = flock'
  where
    flock' = map (updateBoid flock) flock `using` parListChunk numChunks rdeepseq

updateParList :: [Boid] -> [Boid]
updateParList flock = flock'
  where
    flock' = map (updateBoid flock) flock `using` parList rdeepseq