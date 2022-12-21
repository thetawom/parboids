module Sim (runSim, runSimCollect) where

import Boid (Boid, updateBoid)
import Config (Config)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (parList, parListChunk, rdeepseq, rpar, rseq, runEval, using)

--------------------------------------------------------------------------------

data ParStrat = Seq | TwoPart | Chunks Int | ParList

updateWith :: ParStrat -> Config -> [Boid] -> [Boid]
updateWith Seq = updateSeq
updateWith TwoPart = updateTwoPart
updateWith (Chunks n) = updateChunks n
updateWith ParList = updateParList

--------------------------------------------------------------------------------

runSim :: Config -> [Boid] -> Int -> [Boid]
runSim config flock0 nIter = case runSimCollect config flock0 nIter of
  [] -> flock0
  (flockN : _) -> flockN

runSimCollect :: Config -> [Boid] -> Int -> [[Boid]]
runSimCollect cfg flock0 nIter = foldl simLoop [flock0] [1 .. nIter]
  where
    simLoop :: [[Boid]] -> Int -> [[Boid]]
    simLoop [] _ = []
    simLoop flocks@(flock : _) _ = updateWith (Chunks 5) cfg flock : flocks

--------------------------------------------------------------------------------

updateSeq :: Config -> [Boid] -> [Boid]
updateSeq cfg flock = map (updateBoid cfg flock) flock

updateTwoPart :: Config -> [Boid] -> [Boid]
updateTwoPart cfg flock = runEval $ do
  as' <- rpar (force (map (updateBoid cfg flock) as))
  bs' <- rpar (force (map (updateBoid cfg flock) bs))
  _ <- rseq as'
  _ <- rseq bs'
  return (as' ++ bs')
  where
    (as, bs) = splitAt (length flock `div` 2) flock

updateChunks :: Int -> Config -> [Boid] -> [Boid]
updateChunks numChunks cfg flock = flock'
  where
    flock' = map (updateBoid cfg flock) flock `using` parListChunk chunkSize rdeepseq
    chunkSize = length flock' `div` numChunks

updateParList :: Config -> [Boid] -> [Boid]
updateParList cfg flock = flock'
  where
    flock' = map (updateBoid cfg flock) flock `using` parList rdeepseq