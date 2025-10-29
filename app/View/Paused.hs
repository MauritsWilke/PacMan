{-# LANGUAGE RecordWildCards #-}
module View.Paused where
import Graphics.Gloss
import Model

drawPaused :: TileWidth -> Board -> Bool -> Picture
drawPaused tw Board{..} p
  | p = Translate ((fromIntegral width - 5) * (0.5 * tw) + 0.5 * tw) (- (fromIntegral height * (0.5 * tw)) - (0.75 * tw) )
      $ Scale (tw / 250) (tw / 250)
      $ Color white
      $ Text "PAUSED"
  | otherwise = blank