module Main (main) where

import BoidIO (loadFlock, saveFlock)
import Control.Monad (foldM_, unless)
import Sim (runSimCollect)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

hintUsage :: IO a
hintUsage = do
  progname <- getProgName
  die $ "Usage: " ++ progname ++ " <boids-file> <save-dir> <num-iter>"

main :: IO ()
main = do
  args <- getArgs
  (fn, dn, nIter) <- case args of
    [fn, dn, arg2] -> case readMaybe arg2 :: Maybe Int of
      Just nIter -> return (fn, Just dn, nIter)
      Nothing -> hintUsage
    [fn, arg2] -> case readMaybe arg2 :: Maybe Int of
      Just nIter -> return (fn, Nothing, nIter)
      Nothing -> hintUsage
    _ -> hintUsage
  unless (nIter > 0) $ die "num-iter must be a positive integer"
  flock0 <- loadFlock fn
  let flocks = reverse $ runSimCollect flock0 nIter
  foldM_ (saveFlock dn) 0 flocks
  putStrLn "simulation completed"