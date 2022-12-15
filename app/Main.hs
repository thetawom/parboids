module Main (main) where

import Animate (runAnimation)
import BoidIO (loadFlock, saveFlock)
import Config (loadConfig)
import Control.Monad (foldM_, unless)
import GHC.Base (when)
import Sim (runSimCollect)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

hintUsage :: IO a
hintUsage = do
  progname <- getProgName
  die $ "Usage: " ++ progname ++ " <boids-file> <num-iter> [config-file] [output-dir] [-animate]"

main :: IO ()
main = do
  args <- getArgs
  (flockFile, nIter, outputDir, configFile, animate) <- case args of
    [fn, arg2] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Nothing, Nothing, True)
      Nothing -> hintUsage
    [fn, arg2, "-animate"] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Nothing, Nothing, True)
      Nothing -> hintUsage
    [fn, arg2, dn] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Just dn, Nothing, False)
      Nothing -> hintUsage
    [fn, arg2, dn, "-animate"] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Just dn, Nothing, True)
      Nothing -> hintUsage
    [fn, arg2, dn, cfg] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Just dn, Just cfg, False)
      Nothing -> hintUsage
    [fn, arg2, dn, cfg, "-animate"] -> case readMaybe arg2 of
      Just nIter -> return (fn, nIter, Just dn, Just cfg, True)
      Nothing -> hintUsage
    _ -> hintUsage

  unless (nIter > 0) $ die "num-iter must be a positive integer"
  flock0 <- loadFlock flockFile
  config <- loadConfig configFile
  let flocks = reverse $ runSimCollect config flock0 nIter
  foldM_ (saveFlock outputDir) 0 flocks
  putStrLn "simulation complete"
  when animate $ do
    putStrLn "running animation"
    runAnimation flocks
  putStrLn "process complete"