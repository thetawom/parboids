module Animate (runAnimation) where

import Boid (Boid, bPos, bVel)
import Graphics.Gloss
  ( Color,
    Display (InWindow),
    Picture,
    blank,
    blue,
    circleSolid,
    color,
    pictures,
    red,
    simulate,
    translate,
    white,
  )
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Linear.Vector ((*^), (^+^))
import Utils (vScaleTo, vxy)

background :: Color
background = white

window :: Display
window = InWindow "ParBoids" (800, 600) (200, 200)

update :: ViewPort -> Float -> [[Boid]] -> [[Boid]]
update _ _ [] = []
update _ _ (_ : flocks) = flocks

render :: [[Boid]] -> Picture
render [] = blank
render (flock : _) = pictures $ map draw flock

draw :: Boid -> Picture
draw boid =
  pictures
    [ translate x y $ color red $ circleSolid 3,
      translate x' y' $ color blue $ circleSolid 2
    ]
  where
    (x', y') = vxy $ scaleFac *^ bPos boid ^+^ vScaleTo 2 (bVel boid)
    (x, y) = vxy $ scaleFac *^ bPos boid
    scaleFac = 20

runAnimation :: [[Boid]] -> IO ()
runAnimation flocks = simulate window background 60 flocks render update