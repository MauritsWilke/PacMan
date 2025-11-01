module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count

interact :: GameState -> GameState
interact = interactPellets


interactPellets :: GameState -> GameState
interactPellets gs = gs { level = lvl { gameBoard = updatedBoard }, score = updatedScore }
  where
    lvl = level gs
    brd = gameBoard lvl
    plr = player gs
    scr = score gs
    playerPos = bimap floor floor (position plr)

    isAtPellet = GB.get playerPos brd == Just Pellet
    
    updatedBoard = if isAtPellet then GB.set playerPos Empty brd else brd
    updatedScore = if isAtPellet then scr .+ 10 else scr