{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Count (AnimationTimer (AnimationTimer))

positionPlayer :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionPlayer tw (x,y) width height =
  Translate (dx * tw ) (dy * tw)
  where
    dx = x - (0.5 * fromIntegral width)
    dy = y - (0.5 * fromIntegral height)

rotatePlayer :: Direction -> Picture -> Picture
rotatePlayer West  = Rotate 180
rotatePlayer East  = Rotate 0
rotatePlayer South = Rotate 90
rotatePlayer North = Rotate 270

positionArrow :: Float -> Direction -> Picture -> Picture
positionArrow tw dir = Translate dx dy . Rotate rot
  where
    offset = 0.7 * tw
    (dx, dy, rot) = case dir of
      West  -> (-offset, 0, 270)
      East  -> ( offset, 0, 90)
      South -> (0, -offset, 180)
      North -> (0,  offset, 0)

drawPlayer :: AnimationTimer -> Float -> Board -> Player -> Picture
drawPlayer _ _ _ NoPlayer   = blank
drawPlayer (AnimationTimer f) tw Board{..} Player{..} =
  let
    (x, y) = position
  in Pictures
    [ positionPlayer tw (y,x) width height
      $ rotatePlayer direction
      $ if f < 12 then closedMouth tw else openMouth tw
    , positionPlayer tw (y,x) width height
        $ Color red
        $ positionArrow tw queuedDir
        $ Translate (- arrowSize) 0
        $ polygon [(0,0), (arrowSize, arrowSize), (2 * arrowSize, 0)]
    ] where arrowSize = 7

closedMouth :: Float -> Picture
closedMouth tw = Color yellow $ circleSolid (0.5 * tw)

openMouth :: Float -> Picture
openMouth tw =
  Pictures
    [ Color yellow 
      $ circleSolid (0.5 * tw)
    , Color black  
      $ polygon [(0, 0), ( 0.5 * tw,  0.25 * tw), ( 0.5 * tw,- (0.25 * tw))]
    ]

drawPlayerDebug :: Float -> Int -> Board -> Player -> Picture
drawPlayerDebug _ _ _ NoPlayer =
  Color red $ Text "there is no current level"
drawPlayerDebug tw i Board{..} Player{..}
  | i == 2 = let (x, y) = position in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionPlayer tw (y,x) width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ") " ++ show direction)
  | otherwise = blank