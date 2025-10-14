-- Control the user input (main logic)
module Controller where
import Model
import Control.Monad.ST
import Graphics.Gloss.Interface.IO.Game

step :: Float -> ST s (GameState s) -> IO (ST s (GameState s))
step secs gstate = return gstate

input :: Event -> ST s (GameState s) -> IO (ST s (GameState s))
input e gstate = return gstate