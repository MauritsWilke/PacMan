module View.GameOver where
import Graphics.Gloss
import Model

drawGameOver :: TileWidth -> Board -> Bool -> Picture
drawGameOver tw Board{} p
  | p = Pictures $
    Color (makeColor 0 0 0 0.7) (rectangleSolid 10000 10000) 
    : zipWith (\i -> let j = i * (tw / 20) in Translate ((-11.5) * tw - j) j) 
      [0..] -- Gloss does not have bold text, so I'll do it myself
      (replicate 10 (Color white $ Scale (tw / 32) (tw / 32) $ Text "Game Over"))
    ++ 
    [ Translate ((-8.6625) * tw) (- (tw * 2)) 
      $ Color white 
      $ Scale (tw / 128) (tw / 128) 
      $ Text "press 'h' to return to homescreen"
    , Translate ((-6.3) * tw) (- (tw * 4)) 
      $ Color white 
      $ Scale (tw / 128) (tw / 128) 
      $ Text "press 'esc' to rage quit"
    ]
  | otherwise = blank