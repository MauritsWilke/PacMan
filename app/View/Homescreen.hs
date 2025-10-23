module View.Homescreen where
import Graphics.Gloss

renderHomescreen :: Picture
renderHomescreen = Pictures
  [ Translate (-200) 0 $ Color white $ Text "PacMan"
  , Translate (-100) (-50) $ Color white $ Scale 0.2 0.2 $ Text "press space to start"  
  ]