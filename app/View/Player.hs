{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss

positionPlayer :: Float -> (Float,Float) -> Int -> Int -> Picture -> Picture
positionPlayer tw (x,y) width height =
  Translate (dx * tw ) (dy * tw)
  where
    dx = x - (0.5 * fromIntegral width)
    dy = y - (0.5 * fromIntegral height)

positionArrow :: Float -> Direction -> Picture -> Picture
positionArrow tw dir = Translate dx dy . Rotate rot
  where
    offset = 0.7 * tw
    (dx, dy, rot) = case dir of
      West  -> (-offset, 0, 270)
      East  -> ( offset, 0, 90)
      South -> (0, -offset, 180)
      North -> (0,  offset, 0)

drawPlayer :: Float -> Board -> Player -> Picture
drawPlayer _ _ NoPlayer   = blank
drawPlayer l Board{..} Player{..} =
  let
    (x, y) = position
  in Pictures
    [ positionPlayer l (y,x) width height
        $ Color yellow
        $ circleSolid (0.5 * l)
    , positionPlayer l (y,x) width height
        $ Color red
        $ positionArrow l queuedDir
        $ Translate (- arrowSize) 0
        $ polygon [(0,0), (arrowSize, arrowSize), (2 * arrowSize, 0)]
    ] where arrowSize = 7

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