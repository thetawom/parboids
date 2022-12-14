module Utils (bound) where

import Linear.Metric (Metric (norm), normalize)
import Linear.V2 (V2)
import Linear.Vector ((*^))

bound :: Float -> V2 Float -> V2 Float
bound lim v = (norm v `min` lim) *^ normalize v