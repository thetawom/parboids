module Utils (vBound, vScaleTo, vToTup) where

import Control.Lens (view)
import Linear.Metric (Metric (norm), normalize)
import Linear.V2 (V2, _x, _y)
import Linear.Vector ((*^))

vBound :: Float -> V2 Float -> V2 Float
vBound lim v = vScaleTo (norm v `min` lim) v

vScaleTo :: Float -> V2 Float -> V2 Float
vScaleTo n v = n *^ normalize v

vToTup :: V2 a -> (a, a)
vToTup vec = (view _x vec, view _y vec)