-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import View.Level
import View.Player
import View.Score (drawScore)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case scene gstate of
  Homescreen -> blank
  LoadGame -> blank
  ConfigureGame -> blank
  SinglePlayer -> case debugView gstate of
    0 -> viewDefault gstate
    _ -> Pictures [viewDefault gstate, viewDebug gstate]
  MultiPlayer -> blank

viewDefault :: GameState -> Picture
viewDefault gstate
  = Pictures
    [ drawLevel (level gstate)
    , drawPlayer (gameBoard (level gstate)) (player gstate)
    , drawScore (gameBoard (level gstate)) 3333360
    ]

viewDebug :: GameState -> Picture
viewDebug gstate
  = Pictures
    [ drawLevelDebug (debugView gstate) (level gstate)
    , drawPlayerDebug (debugView gstate) (gameBoard (level gstate)) (player gstate)
    ]