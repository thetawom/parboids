module Config (Config (..), loadConfig, show) where

import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import Text.Read (readMaybe)

data Config = Config
  { radius :: Float,
    sn :: Float,
    an :: Float,
    cn :: Float,
    maxVel :: Float,
    wSize :: Maybe Float
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { radius = 3,
      sn = 1.8,
      an = 0.08,
      cn = 0.4,
      maxVel = 2,
      wSize = Nothing
    }

loadConfig :: Maybe String -> IO Config
loadConfig file = case file of
  Just fn -> withFile fn ReadMode readConfigFile
  Nothing -> return defaultConfig

readConfigFile :: Handle -> IO Config
readConfigFile hdl = do
  isEOF <- hIsEOF hdl
  ( if isEOF
      then return defaultConfig
      else
        ( do
            line <- hGetLine hdl
            cfg <- readConfigFile hdl
            let cfg' = case words line of
                  ["radius", arg] -> cfg {radius = read arg}
                  ["sn", arg] -> cfg {sn = read arg}
                  ["an", arg] -> cfg {an = read arg}
                  ["cn", arg] -> cfg {cn = read arg}
                  ["maxVel", arg] -> cfg {maxVel = read arg}
                  ["wSize", arg] -> cfg {wSize = readMaybe arg}
                  _ -> cfg
            return cfg'
        )
    )
