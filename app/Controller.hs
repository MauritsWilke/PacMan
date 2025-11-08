-- Control the user input (main logic)
module Controller where
import Model
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S
import System.Exit (exitSuccess)
import Actions.Move
import Actions.Interact as A
import qualified View.Scenes.SelectBoard as SB
import qualified View.Scenes.LoadSave as LS
import Data.Maybe
import System.Random
import Utils.Count
import Actions.Reset (reset)
import Utils.SaveGame (saveGameState)

-- 
step :: Float -> GameState -> IO GameState
step _ gs
  | shouldQuit gs = exitSuccess
  | shouldSave gs = saveGameState "savegame.json" gs
  | paused gs     = pure (inputKey gs)
  | otherwise     = randomMoves $ A.interact $ inputKey gs

-- Looping input function
input :: Event -> GameState -> IO GameState
input e@(EventKey {})    = return . updateKeyRegister e
input e@(EventResize {}) = return . resize e
input _                  = return

-- Adjust board to resized window
resize :: Event-> GameState -> GameState
resize (EventResize x) gs = gs { screenSize = x }
resize _ gs               = gs


getRandomFrom :: [a] -> IO a
getRandomFrom [] = error "can't get element from empty list"
getRandomFrom as = do
    index <- randomRIO (0,length as -1)
    return $ as !! index

randomMoves :: GameState -> IO GameState
randomMoves gs = do
  let lvl = level gs
      allGhosts = ghosts lvl
      frightened = frightenedGhosts allGhosts
      nonFrightened = filter ((== 0) . getCount . frightTimer ) allGhosts

  -- applyRandom move to all frightened ghosts
  updatedFrightened <- mapM (applyRandom gs) frightened

  let fullGhostList  = updatedFrightened ++ nonFrightened
      updatedLevel   = lvl { ghosts = fullGhostList }
      gs'            = gs { level = updatedLevel }

  return gs'

applyRandom :: GameState -> Ghost -> IO Ghost
applyRandom gs ghost = do
  let avoid = oppositeDirection (ghostDirection ghost)
      allowedDirections = filter (/= avoid) allDirections
      allowedMoves = filter (validMove ghost) allowedDirections

      validMove gh d =
        isJust $ moveIsPossible gs (ghostPosition gh) (ghostSpeed gs) d True

  dir <- case allowedMoves of
    []  -> return $ oppositeDirection (ghostDirection ghost)
    [d] -> return d
    _   -> getRandomFrom allowedMoves

  return (ghostStep gs ghost dir)


keysThatCantRepeat :: [Key]
keysThatCantRepeat = [Char 'p', Char 'w', Char 'a', Char 's', Char 'd']

-- Add or remove keys from active key register
updateKeyRegister :: Event -> GameState -> GameState
updateKeyRegister (EventKey k Down _ _) gs
  | k `elem` keysThatCantRepeat          = applyKey (scene gs) gs k
  | otherwise                            = updateKeyRegister' Down k gs
updateKeyRegister (EventKey k Up _ _) gs = updateKeyRegister' Up k gs
updateKeyRegister _ gs                   = gs

updateKeyRegister' :: KeyState -> Key -> GameState -> GameState
updateKeyRegister' Down k gs = gs { keys = S.insert k (keys gs) }
updateKeyRegister' Up   k gs = gs { keys = S.delete k (keys gs) }

-- For each key that is pressed, apply the action bound to that key
inputKey :: GameState -> GameState
inputKey gs = afterGhostMoves
  where afterKeyInput   = foldl (applyKey (scene gs)) gs (S.toList $ keys gs)
        afterGhostMoves = if scene gs /= SinglePlayer
          then afterKeyInput
          else afterKeyInput
            { level = (level gs) { ghosts = map (ghostMove gs) (ghosts (level gs)) } }

applyKey :: Scene -> GameState -> Key -> GameState
-- META CONTROLS
applyKey _ gs (SpecialKey KeyEsc)               = gs { shouldQuit = True }
applyKey _ gs (Char '0')                        = gs { debugView = 0 }
applyKey _ gs (Char '1')                        = gs { debugView = 1 }
applyKey _ gs (Char '2')                        = gs { debugView = 2 }
applyKey _ gs (Char '3')                        = gs { debugView = 3 }
-- HOMESCREEN 
applyKey Homescreen gs (SpecialKey KeySpace)    = gs { scene = SinglePlayer }
applyKey Homescreen gs (Char 's')               = if (not . null . boards) gs
                                                  then (SB.enterScene gs) { scene = ConfigureGame }
                                                  else gs
applyKey Homescreen gs (Char 'l')               = if (not . null . saves) gs
                                                  then (SB.enterScene gs) { scene = LoadGame }
                                                  else gs
-- CONFIGURE
applyKey ConfigureGame gs (Char 's')            = SB.controlScene (Char 's') gs
applyKey ConfigureGame gs (Char 'w')            = SB.controlScene (Char 'w') gs
applyKey ConfigureGame gs (SpecialKey KeyEnter) = (SB.exitScene gs) { scene = Homescreen }
-- LOAD GAME
applyKey LoadGame gs (Char 's')                 = LS.controlScene (Char 's') gs
applyKey LoadGame gs (Char 'w')                 = LS.controlScene (Char 's') gs
applyKey LoadGame gs (SpecialKey KeyEnter)      = (LS.exitScene gs) { scene = SinglePlayer }
-- MOVEMENT
applyKey SinglePlayer gs (Char 'p')             = gs { paused = not (paused gs), scene = s' }
  where s' = if paused gs then SinglePlayer else Paused
applyKey SinglePlayer gs (Char 'w')             = updatePlayerDir gs North
applyKey SinglePlayer gs (Char 'a')             = updatePlayerDir gs West
applyKey SinglePlayer gs (Char 's')             = updatePlayerDir gs South
applyKey SinglePlayer gs (Char 'd')             = updatePlayerDir gs East
-- PAUSE
applyKey Paused gs (Char 'h')                   = (Actions.Reset.reset gs) { scene = Homescreen }
applyKey Paused gs (Char 's')                   = gs { shouldSave = True }
applyKey Paused gs (Char 'p')                   = gs { paused = not (paused gs), scene = s' }
  where s' = if paused gs then SinglePlayer else Paused
-- CATCH ALL 
applyKey _ gs _                                 = gs