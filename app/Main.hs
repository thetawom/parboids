module Main (main) where

import Animate (runAnimation)
import BoidIO (loadFlock, saveFlock)
import Control.Monad (foldM_, unless)
import Sim (runSimCollect)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

hintUsage :: IO a
hintUsage = do
  progname <- getProgName
  die $ "Usage: " ++ progname ++ " <boids-file> <save-dir> <num-iter> [-animate]"

readIntArg :: String -> Maybe Int
readIntArg arg = readMaybe arg

main :: IO ()
main = do
  args <- getArgs
  (fn, dn, nIter, anim) <- case args of
    [fn, arg2] -> case readIntArg arg2 of
      Just nIter -> return (fn, Nothing, nIter, False)
      Nothing -> hintUsage
    [fn, arg2, "-animate"] -> case readIntArg arg2 of
      Just nIter -> return (fn, Nothing, nIter, True)
      Nothing -> hintUsage
    [fn, dn, arg2] -> case readIntArg arg2 of
      Just nIter -> return (fn, Just dn, nIter, False)
      Nothing -> hintUsage
    [fn, dn, arg2, "-animate"] -> case readIntArg arg2 of
      Just nIter -> return (fn, Just dn, nIter, True)
      Nothing -> hintUsage
    _ -> hintUsage

  unless (nIter > 0) $ die "num-iter must be a positive integer"
  flock0 <- loadFlock fn
  let flocks = reverse $ runSimCollect flock0 nIter
  foldM_ (saveFlock dn) 0 flocks
  putStrLn "simulation complete"
  unless (not anim) $ do
    putStrLn "running animation"
    runAnimation flocks
  putStrLn "process complete"