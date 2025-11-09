{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Count (AnimationTimer (AnimationTimer))

-- | Align player on a tile
positionPlayer :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionPlayer tw (x,y) width height =
  Translate (dx * tw ) (dy * tw)
  where
    dx = x - (0.5 * fromIntegral width)
    dy = y - (0.5 * fromIntegral height)

-- | Rotate player util to ensure looking in the right direction
rotatePlayer :: Direction -> Picture -> Picture
rotatePlayer West  = Rotate 180
rotatePlayer East  = Rotate 0
rotatePlayer South = Rotate 90
rotatePlayer North = Rotate 270

-- | Place memory arrow around pacman
positionArrow :: Float -> Direction -> Picture -> Picture
positionArrow tw dir = Translate dx dy . Rotate rot
  where
    offset = 0.7 * tw
    (dx, dy, rot) = case dir of -- Rotations are different than rotatePlayer !
      West  -> (-offset, 0, 270)
      East  -> ( offset, 0, 90)
      South -> (0, -offset, 180)
      North -> (0,  offset, 0)

-- | Draw the player (and memory arrow) to the board
drawPlayer :: AnimationTimer -> Float -> Board -> Player -> Picture
drawPlayer (AnimationTimer f) tw Board{..} Player{..} =
  let
    (x, y) = position
  in Pictures
    [ positionPlayer tw (y,x) width height
      $ rotatePlayer direction
      $ if f < 12 then closedMouth tw else openMouth tw -- Ensure 5 waka waka's per second
    , positionPlayer tw (y,x) width height
        $ Color red
        $ positionArrow tw queuedDir
        $ Translate (- arrowSize) 0
        $ polygon [(0,0), (arrowSize, arrowSize), (2 * arrowSize, 0)]
    ] where arrowSize = 0.25 * tw

-- | Helper scaler as it is used several times
pacmanSize :: Float
pacmanSize = 0.42

-- | Closed mouth render state
closedMouth :: Float -> Picture
closedMouth tw = Color yellow $ circleSolid (pacmanSize * tw)

-- | Open mouth render state
openMouth :: Float -> Picture
openMouth tw =
  Pictures
    [ closedMouth tw
    , Color black  
      $ polygon [(0, 0), ( pacmanSize * tw,  0.25 * tw), ( pacmanSize * tw,- (0.25 * tw))]
    ]

-- | Draw debug information above Pac-Man (location and direction)
drawPlayerDebug :: Float -> Int -> Board -> Player -> Picture
drawPlayerDebug tw i Board{..} Player{..}
  | i == 2 = let (x, y) = position in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionPlayer tw (y,x) width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ") " ++ show direction)
  | otherwise = blank