-- Control the user input (main logic)
module Controller where
import Model hiding (Down, Up)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)

-- 
step :: Float -> GameState -> IO GameState
step _ gstate 
  | shouldQuit gstate = exitSuccess
  | otherwise         = pure (inputKey gstate)

-- ! CAN CHANGE THE GAMESTATE CURRENTLY
debug :: GameState -> GameState
debug = id

-- Looping input function
input :: Event -> GameState -> IO GameState
input e = return . updateKeyRegister e

-- Add or remove keys from active key register
updateKeyRegister :: Event -> GameState -> GameState
updateKeyRegister (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
updateKeyRegister (EventKey k Up   _ _) gstate = gstate { keys = S.delete k (keys gstate)}
updateKeyRegister _ gstate                     = gstate

-- For each key that is pressed, apply the action bound to that key
inputKey :: GameState -> GameState
inputKey gstate = foldl applyKey gstate (S.toList $ keys gstate)

applyKey :: GameState -> Key -> GameState
applyKey gstate (SpecialKey KeyEsc) = gstate { shouldQuit = True }
applyKey gstate _                   = gstate