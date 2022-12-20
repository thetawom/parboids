module Config (Config (..), WorldSize (..), loadConfig) where

import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)

--------------------------------------------------------------------------------

data Config = Config
  { radius :: Float,
    sn :: Float,
    an :: Float,
    cn :: Float,
    maxVel :: Float,
    wSize :: WorldSize
  }
  deriving (Show)

--------------------------------------------------------------------------------

data WorldSize = Infinite | Size Float

instance Show WorldSize where
  show Infinite = "∞"
  show (Size f) = show f

--------------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig =
  Config
    { radius = 5,
      sn = 1.8,
      an = 0.05,
      cn = 0.3,
      maxVel = 10,
      wSize = Size 40
    }

--------------------------------------------------------------------------------

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
                  ["wSize", arg] -> cfg {wSize = Size $ read arg}
                  _ -> cfg
            return cfg'
        )
    )
