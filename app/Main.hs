module Main where

import Graphics.Gloss.Interface.IO.Game
import Model (initialState)
import View
import Controller

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 400) (0, 0))
       black
       30
       initialState
       view
       input
       step
-- main = playIO (InWindow "Pac-Man" (400, 400) (0, 0))
--               black            -- Background color
--               30               -- Frames per second
--               initialState     -- Initial state
--               view             -- View function
--               input            -- Event function
--               step             -- Step function

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
