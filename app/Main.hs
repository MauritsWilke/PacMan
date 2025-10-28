module Main where

import Graphics.Gloss.Interface.IO.Game
import Model (initialState)
import View
import Controller

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 400) (0, 0))
       black
       60
       initialState
       view
       input
       step