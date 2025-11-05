-- Control the user input (main logic)
{-# LANGUAGE RecordWildCards #-}
module Controller where
import Model
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Actions.Move
-- 
step :: Float -> GameState -> IO GameState
step _ gstate
  | shouldQuit gstate = exitSuccess
  | paused gstate     = pure (inputPause gstate)
  | otherwise         = pure (inputKey gstate)

-- ! CAN CHANGE THE GAMESTATE CURRENTLY
debug :: GameState -> GameState
debug = id

-- Looping input function
input :: Event -> GameState -> IO GameState
input e@(EventKey {})    = return . updateKeyRegister e
input e@(EventResize {}) = return . resize e
input _                  = return

-- Adjust board to resized window
resize :: Event-> GameState -> GameState
resize (EventResize x) gstate = gstate { screenSize = x }
resize _ gstate               = gstate

-- Add or remove keys from active key register
updateKeyRegister :: Event -> GameState -> GameState
updateKeyRegister (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
updateKeyRegister (EventKey k Up   _ _) gstate = gstate { keys = S.delete k (keys gstate)}
updateKeyRegister _ gstate                     = gstate

-- For each key that is pressed, apply the action bound to that key
inputKey :: GameState -> GameState
inputKey gstate = afterKeyInput -- afterGhostMoves
  where afterKeyInput = foldl (applyKey (scene gstate)) gstate (S.toList $ keys gstate)
        afterGhostMoves = afterKeyInput {level = (level gstate) {ghosts = map (ghostMove gstate) (ghosts (level gstate))}}

inputPause :: GameState -> GameState
inputPause gstate = if S.member (Char 'p') (keys gstate)
  then applyKey (scene gstate) gstate (Char 'p')
  else gstate

-- TODO add actions instead of all game logic here
applyKey :: Scene -> GameState -> Key -> GameState
-- META CONTROLS
applyKey _ gstate (SpecialKey KeyEsc)              = gstate { shouldQuit = True }
applyKey _ gstate (Char 'p')                       = gstate { paused = not (paused gstate) }
applyKey _ gstate (Char '0')                       = gstate { debugView = 0 }
applyKey _ gstate (Char '1')                       = gstate { debugView = 1 }
applyKey _ gstate (Char '2')                       = gstate { debugView = 2 }
applyKey _ gstate (Char '3')                       = gstate { debugView = 3 }
-- HOMESCREEN
applyKey Homescreen   gstate (SpecialKey KeySpace) = gstate { scene = SinglePlayer }
-- MOVEMENT
applyKey SinglePlayer gstate (Char 'w')            = gstate { player = playerMove gstate North }
applyKey SinglePlayer gstate (Char 'a')            = gstate { player = playerMove gstate West  }
applyKey SinglePlayer gstate (Char 's')            = gstate { player = playerMove gstate South }
applyKey SinglePlayer gstate (Char 'd')            = gstate { player = playerMove gstate East  }
-- CATCH ALL
applyKey _ gstate _                                = gstate