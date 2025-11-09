-- Convert the game to a picture that can be displayed
module View where

import Graphics.Gloss

import Model
import Utils.Count

import View.Level
import View.Player
import View.Ghost
import View.Score (drawScore)
import View.Scenes.Homescreen (renderHomescreen)
import View.Lives (drawLives)
import View.Paused
import View.Scenes.SelectBoard (renderBoardSelection)
import View.Scenes.LoadSave
import View.GameOver (drawGameOver)
import View.Round

view :: GameState -> IO Picture
view = return . viewPure

-- | Scene management
viewPure :: GameState -> Picture
viewPure gs = case scene gs of
    Homescreen    -> renderHomescreen ((not . null . saves) gs) ((not . null . boards) gs) (screenSize gs) (tileWidth gs)
    LoadGame      -> renderSaveselection gs
    ConfigureGame -> renderBoardSelection gs
    SinglePlayer  -> viewWithOverlay gs
    Paused        -> viewWithOverlay gs
    GameOver      -> viewWithOverlay gs

-- | Helper function as this logic is repeated several times
viewWithOverlay :: GameState -> Picture
viewWithOverlay gs = if debugView gs == 0
                      then viewDefault gs
                      else Pictures [viewDefault gs, viewDebug gs]

-- | The default rendering of the board, player and GUI
-- | If ghosts are frightened, Pac-Man should be on top to eat them
-- | Otherwise, ghosts eat Pac-Man and should be on top
viewDefault :: GameState -> Picture
viewDefault gs =
    let i = if null (frightenedGhosts (ghosts (level gs))) then 2 else 1
    in Pictures $ insertAt i (viewGhosts gs)
       [ drawLevel' gs
       , drawPlayer' gs
       , viewGUI gs
       ]

-- | Extract relevant information and render level
drawLevel' :: GameState -> Picture
drawLevel' gs = drawLevel (tileWidth gs) (level gs)

-- | Extract relevant information and render player
drawPlayer' :: GameState -> Picture
drawPlayer' gs = drawPlayer anim tw brd p
  where
    anim = animation gs
    tw   = tileWidth gs
    brd  = gameBoard (level gs)
    p    = player gs

-- | Render ghosts with relevant information
viewGhosts :: GameState -> Picture
viewGhosts gs = Pictures ghostPics
  where
    anim      = animation gs
    tw        = tileWidth gs
    brd       = gameBoard (level gs)
    ghostList = ghosts (level gs)
    ghostPics = map (drawGhost anim tw brd) ghostList

-- | Show ghost debug information
viewGhostsDebug :: GameState -> Picture
viewGhostsDebug gs = Pictures ghostPics
  where
    tw  = tileWidth gs
    dbg = debugView gs
    brd = gameBoard (level gs)
    ghostList = ghosts (level gs)
    ghostPics = map (drawGhostDebug gs tw dbg brd) ghostList

-- | Render GUI with content to the screen
viewGUI :: GameState -> Picture
viewGUI gs = Pictures
  [ drawScore tw brd (score gs)
  , drawLives tw brd (getCount (lives gs))
  , drawPaused tw brd (scene gs == Paused)
  , drawGameOver tw brd (scene gs == GameOver) (score gs)
  , drawRoundIndicator tw brd (Model.round gs)
  ]
  where
    tw  = tileWidth gs
    brd = gameBoard (level gs)

-- | Helper function to add the ghosts to the right location
insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
    let (before, after) = splitAt i xs
    in before ++ [x] ++ after

-- | Show debug information based on the debug index
viewDebug :: GameState -> Picture
viewDebug gs = Pictures $ insertAt ghostIndex (viewGhostsDebug gs) debugPics
  where
    tw  = tileWidth gs
    dbg = debugView gs
    lvl = level gs
    brd = gameBoard lvl
    p   = player gs
    ghostsLvl  = ghosts lvl
    ghostIndex = if null (frightenedGhosts ghostsLvl) then 2 else 1
    debugPics = [ drawLevelDebug tw dbg lvl, drawPlayerDebug tw dbg brd p]

