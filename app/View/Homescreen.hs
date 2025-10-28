module View.Homescreen where
import Graphics.Gloss

renderHomescreen :: Float -> Picture
renderHomescreen tw = Pictures
  [ Translate ((-7) * tw) 0     
    $ Color white 
    $ Scale (tw / 32) (tw / 32)   
    $ Text "PacMan"
  , Translate ((-5.25) * tw) (- (tw * 2)) 
    $ Color white 
    $ Scale (tw / 128) (tw / 128) 
    $ Text "press space to start"
  ]