module Main (main) where

import Animate (runAnimation)
import BoidIO (loadFlock, saveFlock)
import Config (loadConfig)
import Control.Monad (foldM_, unless)
import GHC.Base (when)
import Options.Applicative
  ( Parser,
    argument,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    str,
    strOption,
    switch,
    (<**>),
  )
import Sim (runSimCollect)
import System.Exit (die)

data Args = Arguments
  { flockFile :: String,
    numIter :: Int,
    outputDir :: Maybe String,
    configFile :: Maybe String,
    animate :: Bool
  }

arguments :: Parser Args
arguments =
  Arguments
    <$> argument str (metavar "FILE" <> help "Initial flock data file")
    <*> option auto (long "num-iter" <> short 'n' <> metavar "INT" <> help "Number of iterations")
    <*> optional (strOption (long "out-dir" <> short 'o' <> metavar "DIR" <> help "Output directory"))
    <*> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG" <> help "Configuration file"))
    <*> switch (long "animate" <> short 'a' <> help "Whether to run animation")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (arguments <**> helper)
        ( fullDesc
            <> progDesc ""
            <> header ""
        )

run :: Args -> IO ()
run args = do
  unless (numIter args > 0) $ die "num-iter must be a positive integer"
  flock0 <- loadFlock $ flockFile args
  config <- loadConfig $ configFile args
  print config
  let flocks = reverse $ runSimCollect config flock0 $ numIter args
  foldM_ (saveFlock $ outputDir args) 0 flocks
  putStrLn "simulation complete"
  when (animate args) $ do
    putStrLn "running animation"
    runAnimation flocks
  putStrLn "process complete"