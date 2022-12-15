module Config (Config (..), loadConfig) where

import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

data Config = Config
  { radius :: Float,
    sn :: Float,
    an :: Float,
    cn :: Float,
    maxVel :: Float
  }

defaultConfig :: Config
defaultConfig =
  Config
    { radius = 3,
      sn = 1.8,
      an = 0.08,
      cn = 0.4,
      maxVel = 2
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
            config <- readConfigFile hdl
            case words line of
              ["radius", arg] -> return config {radius = read arg}
              ["sn", arg] -> return config {sn = read arg}
              ["an", arg] -> return config {an = read arg}
              ["cn", arg] -> return config {cn = read arg}
              ["maxVel", arg] -> return config {maxVel = read arg}
              _ -> return config
        )
    )
