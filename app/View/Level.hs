{-# LANGUAGE RecordWildCards #-}
module View.Level where
import Model
import Utils.Board
import Graphics.Gloss
import qualified Data.IntMap as I

tileWidth :: Float
tileWidth = 25.0

halfTile :: Float
halfTile = 0.5 * tileWidth

tilePosition :: Int -> Int -> Int -> Picture -> Picture
tilePosition width height i = Translate
    (baseX + fromIntegral (i `mod` width) * tileWidth)
    (baseY + fromIntegral (i `div` width) * tileWidth)
  where
    baseX = -(fromIntegral width * halfTile) + halfTile
    baseY = -(fromIntegral height * halfTile) + halfTile

drawLevel :: Level -> Picture
drawLevel NoLevel = blank
drawLevel Level{ gameBoard = Board{..} } =
  Pictures . I.elems $ I.mapWithKey renderTile board
  where
    renderTile i t = tilePosition width height i (tileAsset t)

drawLevelDebug :: Int -> Level -> Picture
drawLevelDebug _ NoLevel = Color red $ Text "there is no current level"
drawLevelDebug i Level{ gameBoard = Board{..} }
  | i <= 0    = blank
  | otherwise = Pictures $ I.elems $ I.mapWithKey renderTile board
  where
    debugTranslation = -halfTile + 0.15 * tileWidth

    renderTile j _ = tilePosition width height j 
      (Translate debugTranslation debugTranslation
        $ Scale 0.08 0.08
        $ Color red
        $ Text (show j))

tileAsset :: Tile -> Picture
tileAsset Wall        = Color blue   $ rectangleWire tileWidth tileWidth
tileAsset Pellet      = Color white  $ rectangleSolid (tileWidth / 4) (tileWidth / 4)
tileAsset PowerPellet = Color white  $ circleSolid (tileWidth / 3)
tileAsset Fruit       = Color red    $ circleSolid (tileWidth / 4)
tileAsset GhostSpawn  = Color green  $ rectangleSolid tileWidth tileWidth
tileAsset GhostExit   = Color orange $ rectangleSolid tileWidth tileWidth
tileAsset Empty       = blank