-- Control the user input (main logic)
module Controller where
import Model
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Actions.Move
import Actions.Interact as A
import View.Scenes.SelectBoard (exitScene, enterScene, controlScene)

-- 
step :: Float -> GameState -> IO GameState
step _ gstate
  | shouldQuit gstate = exitSuccess
  | paused gstate     = pure (inputPause gstate)
  | otherwise         = pure (A.interact (inputKey gstate))

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

updateKeyRegister' :: KeyState -> Key -> GameState -> GameState
updateKeyRegister' Down k gs = gs { keys = S.insert k (keys gs) }
updateKeyRegister' Up   k gs = gs { keys = S.delete k (keys gs) }

keysThatCantRepeat :: [Key]
keysThatCantRepeat = [Char 'p']

-- Add or remove keys from active key register
updateKeyRegister :: Event -> GameState -> GameState
updateKeyRegister (EventKey k Down _ _) gs
  | k `elem` keysThatCantRepeat          = applyKey (scene gs) gs k
  | otherwise                            = updateKeyRegister' Down k gs
updateKeyRegister (EventKey k Up _ _) gs = updateKeyRegister' Up k gs
updateKeyRegister _ gs                   = gs

-- For each key that is pressed, apply the action bound to that key
inputKey :: GameState -> GameState
inputKey gstate = afterGhostMoves
  where afterKeyInput   = foldl (applyKey (scene gstate)) gstate (S.toList $ keys gstate)
        afterGhostMoves = if scene gstate /= SinglePlayer 
          then afterKeyInput 
          else afterKeyInput 
            { level = (level gstate)
               { ghosts = map (ghostMove gstate) (ghosts (level gstate)) }
            }

inputPause :: GameState -> GameState
inputPause gstate = if S.member (SpecialKey KeyEsc) (keys gstate)
  then gstate { shouldQuit = True }
  else gstate

-- TODO add actions instead of all game logic here
applyKey :: Scene -> GameState -> Key -> GameState
-- META CONTROLS
applyKey _ gstate (SpecialKey KeyEsc)               = gstate { shouldQuit = True }
applyKey _ gstate (Char 'p')                        = gstate { paused = not (paused gstate) }
applyKey _ gstate (Char '0')                        = gstate { debugView = 0 }
applyKey _ gstate (Char '1')                        = gstate { debugView = 1 }
applyKey _ gstate (Char '2')                        = gstate { debugView = 2 }
applyKey _ gstate (Char '3')                        = gstate { debugView = 3 }
-- HOMESCREEN 
applyKey Homescreen gstate (SpecialKey KeySpace)    = gstate { scene = SinglePlayer }
applyKey Homescreen gstate (Char 's')               = (enterScene gstate) { scene = ConfigureGame }
-- CONFIGURE 
applyKey ConfigureGame gstate (Char 's')            = controlScene (Char 's') gstate
applyKey ConfigureGame gstate (Char 'w')            = controlScene (Char 'w') gstate
applyKey ConfigureGame gstate (SpecialKey KeyEnter) = (exitScene gstate) { scene = Homescreen }
-- MOVEMENT
applyKey SinglePlayer gstate (Char 'w')             = updatePlayerDir gstate North
applyKey SinglePlayer gstate (Char 'a')             = updatePlayerDir gstate West
applyKey SinglePlayer gstate (Char 's')             = updatePlayerDir gstate South
applyKey SinglePlayer gstate (Char 'd')             = updatePlayerDir gstate East
-- CATCH ALL 
applyKey _ gstate _                                 = gstate