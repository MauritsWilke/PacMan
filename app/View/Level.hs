{-# LANGUAGE RecordWildCards #-}
module View.Level where
import Model
import Utils.Board
import Graphics.Gloss
import qualified Data.IntMap as I

positionTile :: TileWidth -> BoardWidth -> BoardHeight -> Int -> Picture -> Picture
positionTile tw width height i = Translate
    (baseX + fromIntegral (i `mod` width) * tw)
    (baseY + fromIntegral (i `div` width) * tw)
  where
    baseX = -(fromIntegral width * 0.5 * tw) + 0.5 * tw
    baseY = -(fromIntegral height * 0.5 * tw) + 0.5 * tw

drawLevel :: TileWidth -> Level -> Picture
drawLevel _ NoLevel = blank
drawLevel tw Level{ gameBoard = Board{..} } =
  Pictures . I.elems $ I.mapWithKey (renderTile tw) board
  where
    renderTile w i t = positionTile w width height i (tileAsset w t)

drawLevelDebug :: TileWidth -> Int -> Level -> Picture
drawLevelDebug _ _ NoLevel = Color red $ Text "there is no current level"
drawLevelDebug tw i Level{ gameBoard = Board{..} }
  | i == 1    = Pictures $ I.elems $ I.mapWithKey renderTile board
  | i == 3    = Pictures $ I.elems $ I.mapWithKey renderTileCoords board
  | otherwise = blank
  where
    debugTranslation = -(0.5* tw) + 0.15 * tw

    renderTileCoords j _ = positionTile tw width height j
      (Translate debugTranslation (0.5* tw)
        $ Rotate 45
        $ Scale 0.08 0.08
        $ Color red
        $ Text (show (indexToCoord j width)))

    renderTile j _ = positionTile tw width height j
      (Translate debugTranslation debugTranslation
        $ Scale 0.08 0.08
        $ Color red
        $ Text (show j))

tileAsset :: TileWidth -> Tile -> Picture
tileAsset l Wall        = Color blue   $ rectangleWire l l
tileAsset l Pellet      = Color white  $ rectangleSolid (l / 4) (l / 4)
tileAsset l PowerPellet = Color white  $ circleSolid (l / 3)
tileAsset l Fruit       = Color red    $ circleSolid $ l / 4
tileAsset l GhostSpawn  = Color green  $ rectangleSolid l l
tileAsset l GhostExit   = Color orange $ rectangleSolid l l
tileAsset _ PlayerSpawn = blank
tileAsset _ Empty       = blank