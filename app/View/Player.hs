{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Board

positionPlayer :: Int -> Int -> Int -> Int -> Picture -> Picture
positionPlayer x y width height =
  Translate (fromIntegral x - (0.5 * fromIntegral width)) (fromIntegral y - (0.5 * fromIntegral height))  

drawPlayer :: Board -> Player -> Picture
drawPlayer _ NoPlayer   = blank
drawPlayer Board{..} Player{..} = let (x, y) = tilePosition in
  positionPlayer x y width height
    $ Color yellow
    $ circleSolid 10

drawPlayerDebug :: Int -> Board -> Player -> Picture
drawPlayerDebug _ _ NoPlayer = 
  Color red $ Text "there is no current level"
drawPlayerDebug i Board{..} Player{..}
  | i == 2 = let (x, y) = tilePosition in
    positionPlayer x y width height
      $ Scale 0.08 0.08
      $ Color red
      $ Text ("(" ++ show x ++ ", " ++ show y ++ ")")
  | otherwise = blank