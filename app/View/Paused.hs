{-# LANGUAGE RecordWildCards #-}
module View.Paused where
import Graphics.Gloss
import Model
import Utils.Text

drawPaused :: TileWidth -> Board -> Bool -> Picture
drawPaused tw Board{..} p
  | p = Pictures $
    Color (makeColor 0 0 0 0.7) (rectangleSolid 10000 10000) 
    : zipWith (\i -> let j = i * (tw / 20) in Translate ((-7.5) * tw - j) j) 
      [0..] -- Gloss does not have bold text, so I'll do it myself
      (replicate 10 (Color white $ Scale (tw / 32) (tw / 32) $ Text "PAUSED"))
    ++ 
    [ Translate ((-7.6125) * tw) (- (tw * 2)) 
      $ Color white 
      $ Scale (tw / 128) (tw / 128) 
      $ Text "press 'p' to continue playing"
    , Translate ((-7.35) * tw) (- (tw * 4)) 
      $ Color white 
      $ Scale (tw / 128) (tw / 128) 
      $ Text "press 'esc' to exit the game"
    ]
  | otherwise = blank