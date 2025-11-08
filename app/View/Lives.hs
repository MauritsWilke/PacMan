{-# LANGUAGE RecordWildCards #-}

module View.Lives where
import Graphics.Gloss
import Model

drawLives :: TileWidth -> Board -> Int -> Picture
drawLives _ _ 0          = blank
drawLives tw b@Board{..} l 
  | l < 0 = blank
  | otherwise = Pictures 
  [ Translate (fromIntegral (l - 1) * tw) 0
  $ Translate baseTranslationX baseTranslationY
  $ Color yellow
  $ circleSolid liveSize
  , drawLives tw b (l - 1)
  ] where baseTranslationX = - (fromIntegral width * (0.5 * tw) - 0.5 * tw)
          baseTranslationY = - (fromIntegral height * (0.5 * tw)) - 0.75 * tw
          liveSize         = 0.4 * tw