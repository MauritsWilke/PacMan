{-# LANGUAGE RecordWildCards #-} -- godsent
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import Utils.Board
import qualified Data.IntMap.Lazy as I

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures
  [
    drawLevel (level gstate),
    drawPlayer (player gstate)
  ]

tileWidth :: Float
tileWidth = 25.0

drawLevel :: Level -> Picture
drawLevel NoLevel = Color red $ rectangleSolid 10 10
drawLevel Level{ gameBoard = Board{..} } =
  Pictures . I.elems $ I.mapWithKey tilePic board
  where
    -- The 0.5 tilewidth is to properly center tiles
    baseX = -(fromIntegral width * tileWidth / 2) + (0.5 * tileWidth)
    baseY = -(fromIntegral height * tileWidth / 2) +  (0.5 * tileWidth)
    tilePic i t = Translate
      (baseX + fromIntegral (i `mod` width) * tileWidth)
      (baseY + fromIntegral (i `div` width) * tileWidth)
      (tileAsset t)

tileAsset :: Tile -> Picture
tileAsset Wall        = Color blue  $ rectangleSolid tileWidth tileWidth
tileAsset Empty       = blank
tileAsset Pellet      = Color white $ rectangleSolid (tileWidth / 4) (tileWidth / 4)
tileAsset PowerPellet = Color white $ circleSolid (tileWidth / 3)
tileAsset Fruit       = Color red   $ circleSolid (tileWidth / 4)
tileAsset GhostSpawn  = Color green $ rectangleSolid tileWidth tileWidth

drawPlayer :: Player -> Picture
drawPlayer NoPlayer = blank
drawPlayer Player{..} = Color yellow $ circleSolid 10 -- TODO does not take the player position into account now