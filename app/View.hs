-- Convert the game to a picture that can be displayed
module View where

import Model (GameState)
import Graphics.Gloss


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure _ = circle 10