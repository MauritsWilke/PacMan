-- Convert the game to a picture that can be displayed
module View where

import Model (GameState)
import Graphics.Gloss
import Control.Monad.ST

view :: ST s (GameState s) -> IO Picture
view = return . viewPure

viewPure :: ST s (GameState s) -> Picture
viewPure gstate = circle 10