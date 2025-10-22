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
viewPure gstate
  | debugView gstate == 0 = viewDefault gstate
  | otherwise             = Pictures [viewDefault gstate, viewDebug gstate]

viewDefault :: GameState -> Picture
viewDefault gstate = Pictures
      [
        drawLevel (level gstate),
        drawPlayer (player gstate)
      ]

viewDebug :: GameState -> Picture
viewDebug gstate = Pictures
  [
    drawLevelDebug (debugView gstate) (level gstate)
  ]

tileWidth :: Float
tileWidth = 25.0


-- TODO refactor these into own file and make DRY
drawLevel :: Level -> Picture
drawLevel NoLevel = Color red $ rectangleSolid 10 10
drawLevel Level{ gameBoard = Board{..} } =
  Pictures . I.elems $ I.mapWithKey tilePic board
  where
    -- The 0.5 tilewidth is to properly center tiles
    halfTile = 0.5 * tileWidth
    baseX = -(fromIntegral width * tileWidth / 2) + halfTile
    baseY = -(fromIntegral height * tileWidth / 2) + halfTile
    tilePic i t = Translate
      (baseX + fromIntegral (i `mod` width) * tileWidth)
      (baseY + fromIntegral (i `div` width) * tileWidth)
      (tileAsset t)

drawLevelDebug :: Int -> Level -> Picture
drawLevelDebug _ NoLevel                        = blank
drawLevelDebug i Level{ gameBoard = Board{..} } 
  | i <= 0 = blank
  | otherwise = Pictures . I.elems $ I.mapWithKey tilePic board
    where
      -- The 0.5 tilewidth is to properly center tiles
      halfTile = 0.5 * tileWidth
      debugTranslation = -halfTile + 0.15 * tileWidth
      baseX = -(fromIntegral width * tileWidth / 2) + halfTile
      baseY = -(fromIntegral height * tileWidth / 2) + halfTile
      tilePic j _ = Translate
        (baseX + fromIntegral (j `mod` width) * tileWidth)
        (baseY + fromIntegral (j `div` width) * tileWidth)
        (Translate debugTranslation debugTranslation $ Scale 0.08 0.08 $ Color red $ Text (show j))


tileAsset :: Tile -> Picture
tileAsset Wall        = Color blue  $ rectangleWire tileWidth tileWidth
tileAsset Empty       = blank
tileAsset Pellet      = Color white  $ rectangleSolid (tileWidth / 4) (tileWidth / 4)
tileAsset PowerPellet = Color white  $ circleSolid (tileWidth / 3)
tileAsset Fruit       = Color red    $ circleSolid (tileWidth / 4)
tileAsset GhostSpawn  = Color green  $ rectangleSolid tileWidth tileWidth
tileAsset GhostExit   = Color orange $ rectangleSolid tileWidth tileWidth

drawPlayer :: Player -> Picture
drawPlayer NoPlayer = blank
drawPlayer Player{..} = Color yellow $ circleSolid 10 -- TODO does not take the player position into account now