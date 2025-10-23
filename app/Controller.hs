-- Control the user input (main logic)
{-# LANGUAGE RecordWildCards #-}
module Controller where
import Model
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
input e@(EventKey {}) = return . updateKeyRegister e
input e@(EventResize {}) = return . resize e
input _ = return

-- Adjust board to resized window
resize :: Event-> GameState -> GameState
resize (EventResize x) gstate = gstate {size = x}
resize _ gstate = gstate

-- Add or remove keys from active key register
updateKeyRegister :: Event -> GameState -> GameState
updateKeyRegister (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
updateKeyRegister (EventKey k Up   _ _) gstate = gstate { keys = S.delete k (keys gstate)}
updateKeyRegister _ gstate                     = gstate

-- For each key that is pressed, apply the action bound to that key
inputKey :: GameState -> GameState
inputKey gstate = foldl applyKey gstate (S.toList $ keys gstate)

applyKey :: GameState -> Key -> GameState
-- META CONTROLS
applyKey gstate (SpecialKey KeyEsc) = gstate { shouldQuit = True }
applyKey gstate (Char '0')          = gstate { debugView = 0 }
applyKey gstate (Char '1')          = gstate { debugView = 1 }
applyKey gstate (Char '2')          = gstate { debugView = 2 }
applyKey gstate (Char '3')          = gstate { debugView = 3 }
-- MOVEMENT
applyKey gstate (Char 'w')          = gstate { player = movePlayer North (player gstate) }
applyKey gstate (Char 'a')          = gstate { player = movePlayer West  (player gstate) }
applyKey gstate (Char 's')          = gstate { player = movePlayer South (player gstate) }
applyKey gstate (Char 'd')          = gstate { player = movePlayer East  (player gstate) }
-- CATCH ALL
applyKey gstate _                   = gstate

movePlayer :: Direction -> Player -> Player
movePlayer _ NoPlayer       = NoPlayer
movePlayer d p = let (x, y) = tilePosition p in case d of
  North -> p { tilePosition = (x    , y + 1) } -- add offset calculation
  South -> p { tilePosition = (x    , y - 1) }
  East  -> p { tilePosition = (x + 1, y    ) }
  West  -> p { tilePosition = (x - 1, y    ) }