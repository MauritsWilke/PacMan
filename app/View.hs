-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import View.Level
import View.Player
import View.Ghost
import View.Score (drawScore)
import View.Homescreen (renderHomescreen)
import View.Lives (drawLives)
import View.Paused

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case scene gstate of
  Homescreen    -> renderHomescreen (tileWidth gstate)
  LoadGame      -> blank
  ConfigureGame -> blank
  SinglePlayer  -> case debugView gstate of
                0 -> viewDefault gstate
                _ -> Pictures [viewDefault gstate, viewDebug gstate]
  MultiPlayer   -> blank

viewDefault :: GameState -> Picture
viewDefault gstate
  = Pictures $
    [ drawLevel t (level gstate)
    , drawPlayer t b (player gstate)
    ] 
    ++
    ghostPictures 
    ++
    [ drawScore t b (score gstate)
    , drawLives t b 3
    , drawPaused t b (paused gstate)
    ]
  where 
    ghostPictures = map (drawGhost t b) (ghosts (level gstate))
    t = tileWidth gstate
    b = gameBoard (level gstate)

viewDebug :: GameState -> Picture
viewDebug gstate
  = Pictures
    [ drawLevelDebug (tileWidth gstate) (debugView gstate) (level gstate)
    , drawPlayerDebug (tileWidth gstate) (debugView gstate) (gameBoard (level gstate)) (player gstate)
    ]