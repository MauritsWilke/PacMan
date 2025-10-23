{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Board


positionPlayer :: Float -> Int -> Int -> Int -> Int -> Picture -> Picture
positionPlayer tileW x y width height =
  Translate (dx * tileW  + 0.5 * tileW ) (dy * tileW + 0.5* tileW )
  where
    dx = fromIntegral x - (0.5 * fromIntegral width)
    dy = fromIntegral y - (0.5 * fromIntegral height)

drawPlayer :: Float -> Board -> Player -> Picture
drawPlayer _ _ NoPlayer   = blank
drawPlayer l Board{..} Player{..} = let (x, y) = tilePosition in
  positionPlayer l x y width height
    $ Color yellow
    $ circleSolid (0.5*l)

drawPlayerDebug :: Float -> Int -> Board -> Player -> Picture
drawPlayerDebug _ _ _ NoPlayer =
  Color red $ Text "there is no current level"
drawPlayerDebug tileW i Board{..} Player{..}
  | i == 2 = let (x, y) = tilePosition in
    Translate (- (0.5 * tileW)) (0.5 * tileW)
      $ positionPlayer tileW x y width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ")")
  | otherwise = blank