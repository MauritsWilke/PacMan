{-# LANGUAGE RecordWildCards #-}
module View.Ghost where
import Model
import Graphics.Gloss

positionGhost :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionGhost tw (x,y) width height =
  Translate (dx * tw ) (dy * tw)
  where
    dx = x - (0.5 * fromIntegral width)
    dy = y - (0.5 * fromIntegral height)



drawGhost :: TileWidth -> Board -> Ghost -> Picture
drawGhost l Board{..} Ghost{..} = let (x, y) = ghostPosition in
  positionGhost l (y,x) width height
    $ Color (getGhostColor ghostType)
    $ circleSolid (0.5 * l)
-- for each ghost -> draw

getGhostColor :: GhostType -> Color
getGhostColor Inky = cyan
getGhostColor Blinky = red
getGhostColor Pinky = rose
getGhostColor Clyde = orange


drawGhostDebug :: TileWidth -> Int -> Board -> Ghost -> Picture
drawGhostDebug tw i Board{..} Ghost{..}
  | i == 2 = let (x, y) = ghostPosition in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionGhost tw (y,x) width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ")")
  | otherwise = blank