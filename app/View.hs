-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import View.Level
import View.Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate
  | debugView gstate == 0 = viewDefault gstate
  | otherwise             = Pictures [viewDefault gstate, viewDebug gstate]

viewDefault :: GameState -> Picture
viewDefault gstate
  = Pictures
    [ drawLevel (tileWidth gstate) (level gstate)
    , drawPlayer (tileWidth gstate) (gameBoard (level gstate)) (player gstate)
    ]

viewDebug :: GameState -> Picture
viewDebug gstate
  = Pictures
    [ drawLevelDebug (tileWidth gstate) (debugView gstate) (level gstate)
    , drawPlayerDebug (tileWidth gstate) (debugView gstate) (gameBoard (level gstate)) (player gstate)
    ]