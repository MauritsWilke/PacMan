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
    drawLevel (level gstate)
  ]

tileWidth :: Float
tileWidth = 40.0

drawLevel :: Level -> Picture
drawLevel NoLevel = Color red $ rectangleSolid 10 10
drawLevel Level{ gameBoard = Board{..}, ..} = Pictures . I.elems $ I.mapWithKey tileToPicture board
  where
    baseX :: Float
    baseX = - ((fromIntegral width * tileWidth) / 2)
    baseY :: Float
    baseY = - ((fromIntegral height * tileWidth) / 2)

    tileToPicture :: Int -> Tile -> Picture
    tileToPicture i Pellet
      = Translate
        (baseX + fromIntegral (i `mod` width) * tileWidth) -- X translation
        (baseY + fromIntegral (i `div` width) * tileWidth) -- Y translation
      $ Color (if even (i `div` 10) then (if even i then red else green) else (if even i then green else red))
      $ tileAsset Pellet

    tileToPicture _ _    = blank

tileAsset :: Tile -> Picture
tileAsset Wall        = Color blue   $ rectangleSolid tileWidth tileWidth
tileAsset Empty       = Color green  $ rectangleSolid tileWidth tileWidth
tileAsset Pellet      = Color yellow $ circle (tileWidth / 3)
tileAsset PowerPellet = Color orange $ rectangleSolid tileWidth tileWidth
tileAsset Fruit       = Color yellow $ rectangleSolid tileWidth tileWidth