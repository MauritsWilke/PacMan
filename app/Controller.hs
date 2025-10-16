-- Control the user input (main logic)
module Controller where
import Model
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step _ = return

input :: Event -> GameState -> IO GameState
input _ = return