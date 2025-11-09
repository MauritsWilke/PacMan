{-# LANGUAGE RecordWildCards #-}
module View.Ghost where
import Utils.Count (getCount, AnimationTimer (AnimationTimer))
import Model
import Graphics.Gloss
import Data.Maybe
import View.Player (rotatePlayer)

type TileWidth = Float

positionGhost :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionGhost tw (x, y) width height = Translate (dx * tw ) (dy * tw)
  where dx = x - (0.5 * fromIntegral width)
        dy = y - (0.5 * fromIntegral height)

drawGhost :: AnimationTimer -> Float -> Board -> Ghost -> Picture
drawGhost (AnimationTimer f) tw Board{..} Ghost{..} = let (x, y) = ghostPosition in
  positionGhost tw (y,x) width height
    $ Color ghostColor
    $ ghostShape f tw ghostDirection
  where ghostColor = getGhostColor Ghost{..}

getGhostColor :: Ghost -> Color
getGhostColor Ghost{..}
 | getCount releaseTimer > 0                = regularGhostColor Ghost{..}
 | isJust destination && ghostMode == Spawn = makeColor 1 1 1 0 -- only eyes remain when eaten
 | fright > 0                               = blinkFrightTimer fright
 | otherwise                                = regularGhostColor Ghost{..}
  where fright = getCount frightTimer

blinkFrightTimer :: Int -> Color
blinkFrightTimer i
  | i > 120 = blue
  | even (i `div` 10) = blue
  | otherwise = white

regularGhostColor :: Ghost -> Color
regularGhostColor Ghost{..} = case ghostType of
    Inky   -> cyan
    Blinky -> red
    Pinky  -> rose
    Clyde  -> orange

drawGhostDebug :: GameState -> Float -> Int -> Board -> Ghost -> Picture
drawGhostDebug _ tw i Board{..} Ghost{..}
  | i == 2 = let (x, y) = ghostPosition in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionGhost tw (y, x) width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ") " ++ show ghostDirection ++ show destination )
  | otherwise = blank

ghostEye :: Direction -> Float -> Picture
ghostEye dir tw = rotatePlayer dir $ Pictures
  [ Color white
    $ circleSolid (0.12 * tw)
  , Color black
    $ Translate (0.04 * tw) 0
    $ circleSolid (0.06 * tw )
  ]

ghostShape :: Int -> Float -> Direction -> Picture
ghostShape f tw dir =
  let
    body = Pictures
      [ Translate 0 (0.1 * tw) $ circleSolid (0.3 * tw) -- 6px width and 1px up from center
      , Translate 0 (- (0.1 * tw)) $ rectangleSolid (0.6 * tw) (0.2 * tw) -- 6px w 3px h 1px up
      , Translate (- (0.5 * tw)) (- (0.45 * tw)) $
        if f < 12 then wave
        else wave'
      ]

    eyes = Pictures
      [ Translate (- (0.15 * tw)) (0.2 * tw) $ ghostEye dir tw
      , Translate (0.15 * tw) (0.2 * tw) $ ghostEye dir tw
      ]

    wave = polygon
      [ (0.20 * tw, 0.1 * tw)
      , (0.35 * tw, 0.3 * tw)
      , (0.50 * tw, 0.1 * tw)
      , (0.65 * tw, 0.3 * tw)
      , (0.80 * tw, 0.1 * tw)
      , (0.80 * tw, 0.3 * tw)
      , (0.20 * tw, 0.3 * tw)
      ]
    wave' = polygon
      [ (0.20 * tw, 0.3 * tw)
      , (0.35 * tw, 0.1 * tw)
      , (0.50 * tw, 0.3 * tw)
      , (0.65 * tw, 0.1 * tw)
      , (0.80 * tw, 0.3 * tw)
      , (0.20 * tw, 0.3 * tw)
      ]

  in Pictures [body, eyes]
