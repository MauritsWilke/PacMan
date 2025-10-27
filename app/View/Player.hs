{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Board


positionPlayer :: Float -> Int -> Int -> Int -> Int -> Picture -> Picture
positionPlayer tw x y width height =
  Translate (dx * tw  + 0.5 * tw ) (dy * tw + 0.5* tw )
  where
    dx = fromIntegral x - (0.5 * fromIntegral width)
    dy = fromIntegral y - (0.5 * fromIntegral height)

drawPlayer :: Float -> Board -> Player -> Picture
drawPlayer _ _ NoPlayer   = blank
drawPlayer l Board{..} Player{..} = let (x, y) = tilePosition in
  positionPlayer l y x width height
    $ Color yellow
    $ circleSolid (0.5 * l)

drawPlayerDebug :: Float -> Int -> Board -> Player -> Picture
drawPlayerDebug _ _ _ NoPlayer =
  Color red $ Text "there is no current level"
drawPlayerDebug tw i Board{..} Player{..}
  | i == 2 = let (x, y) = tilePosition in
    Translate (- (0.5 * tw)) (0.5 * tw)
      $ positionPlayer tw y x width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ")")
  | otherwise = blank