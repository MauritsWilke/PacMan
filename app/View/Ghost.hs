{-# LANGUAGE RecordWildCards #-}
module View.Ghost where
import Utils.Count (getCount)
import Model
import Graphics.Gloss
import Actions.Move (goalAlgorithm)
import Data.Maybe

type TileWidth = Float

positionGhost :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionGhost tw (x,y) width height =
  Translate (dx * tw ) (dy * tw)
  where
    dx = x - (0.5 * fromIntegral width)
    dy = y - (0.5 * fromIntegral height)



drawGhost :: Float -> Board -> Ghost -> Picture
drawGhost l Board{..} Ghost{..} = let (x, y) = ghostPosition in
  positionGhost l (y,x) width height
    $ Color ghostColor
    $ circleSolid (0.5 * l)
  where ghostColor = getGhostColor Ghost{..}--ghostType
-- for each ghost -> draw

getGhostColor :: Ghost -> Color
getGhostColor Ghost{..} 
 | isJust respawning        = dark green
 | getCount frightTimer > 0 = blue
 | otherwise = case ghostType of
    Inky   -> cyan
    Blinky -> red
    Pinky  -> rose
    Clyde  -> orange


drawGhostDebug :: GameState -> Float -> Int -> Board -> Ghost -> Picture
drawGhostDebug gs tw i Board{..} g@Ghost{..}
  | i == 2 = let (x, y) = ghostPosition in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionGhost tw (y,x) width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ") " ++ show ghostDirection ++ show (goalAlgorithm gs g) )
  | otherwise = blank