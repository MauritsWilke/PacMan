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
tileWidth = 25.0

drawLevel :: Level -> Picture
drawLevel NoLevel = Color red $ rectangleSolid 10 10
drawLevel Level{ gameBoard = Board{..}, ..} = Pictures . I.elems $ I.mapWithKey tileToPicture board
  where
    baseX = - ((fromIntegral width * tileWidth) / 2)
    baseY = - ((fromIntegral height * tileWidth) / 2)

    tileToPicture :: Int -> Tile -> Picture
    tileToPicture i a 
      = Translate
        (baseX + fromIntegral (i `mod` width) * tileWidth) -- X translation
        (baseY + fromIntegral (i `div` width) * tileWidth) -- Y translation
      $ tileAsset a

tileAsset :: Tile -> Picture
tileAsset Wall        = Color blue  $ rectangleSolid tileWidth tileWidth
tileAsset Empty       = blank
tileAsset Pellet      = Color white $ rectangleSolid (tileWidth / 4) (tileWidth / 4)
tileAsset PowerPellet = Color white $ circleSolid (tileWidth / 3)
tileAsset Fruit       = Color red   $ circleSolid (tileWidth / 4)
tileAsset GhostSpawn  = Color green $ rectangleSolid tileWidth tileWidth