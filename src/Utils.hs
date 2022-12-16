module Utils (vBound, vScaleTo, vx, vy, vxy, wrapDisp, vWrap) where

import Data.Fixed (mod')
import Linear.Metric (Metric (norm), normalize)
import Linear.V2 (V2 (V2))
import Linear.Vector ((*^))

--------------------------------------------------------------------------------

vBound :: Float -> V2 Float -> V2 Float
vBound lim v = vScaleTo (norm v `min` lim) v

vScaleTo :: Float -> V2 Float -> V2 Float
vScaleTo n v = n *^ normalize v

vWrap :: Float -> V2 Float -> V2 Float
vWrap size (V2 x y) = V2 (wrap x) (wrap y)
  where
    wrap a = (a + size / 2) `mod'` size - (size / 2)

--------------------------------------------------------------------------------

vx :: V2 a -> a
vx (V2 x _) = x

vy :: V2 a -> a
vy (V2 _ y) = y

vxy :: V2 a -> (a, a)
vxy (V2 x y) = (x, y)

--------------------------------------------------------------------------------

wrapDisp :: Float -> V2 Float -> V2 Float -> V2 Float
wrapDisp size p1 p2 = V2 dx' dy'
  where
    dx'
      | abs dx > 0.5 * size = dx + (if x2 > x1 then -size else size)
      | otherwise = dx
    dy'
      | abs dy > 0.5 * size = dy + (if y2 > y1 then -size else size)
      | otherwise = dy
    dx = x2 - x1
    dy = y2 - y1
    (V2 x1 y1) = vWrap size p1
    (V2 x2 y2) = vWrap size p2