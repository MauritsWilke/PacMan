{-# LANGUAGE RecordWildCards #-}
module View.Level where
import Model
import Utils.Board
import Graphics.Gloss
import qualified Data.IntMap as I

positionTile :: Float -> Int -> Int -> Int -> Picture -> Picture
positionTile tileW width height i = Translate
    (baseX tileW + fromIntegral (i `mod` width) * tileW)
    (baseY tileW + fromIntegral (i `div` width) * tileW)
  where
    baseX tileW = -(fromIntegral width * 0.5* tileW) + 0.5* tileW
    baseY tileW = -(fromIntegral height * 0.5* tileW) + 0.5* tileW

drawLevel :: Float -> Level -> Picture
drawLevel _ NoLevel = blank
drawLevel tileW Level{ gameBoard = Board{..} } =
  Pictures . I.elems $ I.mapWithKey (renderTile tileW) board
  where
    renderTile tileW i t = positionTile tileW width height i (tileAsset tileW t)

drawLevelDebug :: Float -> Int -> Level -> Picture
drawLevelDebug _ _ NoLevel = Color red $ Text "there is no current level"
drawLevelDebug tileW i Level{ gameBoard = Board{..} }
  | i == 1    = Pictures $ I.elems $ I.mapWithKey (renderTile tileW) board
  | i == 3    = Pictures $ I.elems $ I.mapWithKey (renderTileCoords tileW) board
  | otherwise = blank
  where
    debugTranslation tileW = -(0.5* tileW) + 0.15 * tileW

    renderTileCoords tileW j _ = positionTile tileW width height j
      (Translate (debugTranslation tileW) (0.5* tileW)
        $ Rotate 45
        $ Scale 0.08 0.08
        $ Color red
        $ Text (show (indexToCoord j width)))

    renderTile tileW j _ = positionTile tileW width height j
      (Translate (debugTranslation tileW) (debugTranslation tileW)
        $ Scale 0.08 0.08
        $ Color red
        $ Text (show j))

tileAsset :: Float -> Tile -> Picture
tileAsset l Wall        = Color blue   $ rectangleWire l l
tileAsset l Pellet      = Color white  $ rectangleSolid (l/4) (l/4)
tileAsset l PowerPellet = Color white  $ circleSolid (l/3)
tileAsset l Fruit       = Color red    $ circleSolid $ l /4
tileAsset l GhostSpawn  = Color green  $ rectangleSolid l l
tileAsset l GhostExit   = Color orange $ rectangleSolid l l
tileAsset _ Empty       = blank