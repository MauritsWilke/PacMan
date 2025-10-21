{-# LANGUAGE RecordWildCards #-} -- godsent

-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import Utils.Board
import qualified Data.IntMap.Lazy as I
import Data.IntMap (Key)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures
  [
    drawLevel (level gstate)
  ]

tileWidth :: Float
tileWidth = 10.0

drawLevel :: Level -> Picture
drawLevel NoLevel = Color red $ rectangleSolid 10 10
drawLevel Level{ gameBoard = Board{..}, ..} = Pictures . I.elems $ I.mapWithKey tileToPicture board
  where
    baseX :: Float
    baseX = - ((fromIntegral width) * tileWidth)
    baseY :: Float
    baseY = - ((fromIntegral height) * tileWidth)

    tileToPicture :: Int -> Tile -> Picture
    tileToPicture i Pellet
      = Translate 
        (baseX + fromIntegral (i `mod` width) * tileWidth) -- X translation
        (baseY + fromIntegral (i `div` width) * tileWidth) -- Y translation
      $ Color (if even i then red else green)
      $ rectangleSolid tileWidth tileWidth

    tileToPicture _ _    = blank



-- tileToPicture Wall        = Color red $ circle tileWidth
-- tileToPicture Empty       = Color green $ circle tileWidth
-- tileToPicture Pellet      = Color blue $ circle tileWidth
-- tileToPicture PowerPellet = Color orange $ circle tileWidth
-- tileToPicture Fruit       = Color yellow $ circle tileWidth