-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import View.Level
import View.Player
import View.Score (drawScore)
import View.Homescreen (renderHomescreen)
import View.Lives (drawLives)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case scene gstate of
  Homescreen -> renderHomescreen (tileWidth gstate)
  LoadGame -> blank
  ConfigureGame -> blank
  SinglePlayer -> case debugView gstate of
    0 -> viewDefault gstate
    _ -> Pictures [viewDefault gstate, viewDebug gstate]
  MultiPlayer -> blank

viewDefault :: GameState -> Picture
viewDefault gstate
  = Pictures
    [ drawLevel (tileWidth gstate) (level gstate)
    , drawPlayer (tileWidth gstate) (gameBoard (level gstate)) (player gstate)
    , drawScore (tileWidth gstate) (gameBoard (level gstate)) 3333360
    , drawLives (tileWidth gstate) (gameBoard (level gstate)) 3
    ]

viewDebug :: GameState -> Picture
viewDebug gstate
  = Pictures
    [ drawLevelDebug (tileWidth gstate) (debugView gstate) (level gstate)
    , drawPlayerDebug (tileWidth gstate) (debugView gstate) (gameBoard (level gstate)) (player gstate)
    ]