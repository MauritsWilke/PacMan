{-# LANGUAGE RecordWildCards #-}
module View.Player where
import Model
import Graphics.Gloss
import Utils.Board

drawPlayer :: Player -> Picture
drawPlayer NoPlayer   = blank
drawPlayer Player{..} = let (x, y) = tilePosition in
  Translate (fromIntegral x * tileWidth) (fromIntegral y * tileWidth)
  $ Color yellow
  $ circleSolid 10

drawPlayerDebug :: Int -> Player -> Picture
drawPlayerDebug _ NoPlayer = Color red $ Text "there is no current level"
drawPlayerDebug i Player{..}
  | i == 2 = let (x, y) = tilePosition in
  Translate (fromIntegral x * tileWidth - halfTile) (fromIntegral y * tileWidth + halfTile)
  $ Scale 0.08 0.08
  $ Color red
  $ Text ("(" ++ show x ++ ", " ++ show y ++ ")")
  | otherwise = blank