module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count

interact :: GameState -> GameState
interact = interactPellets

interactPellets :: GameState -> GameState
interactPellets gs 
  = gs 
  { level        = lvl { gameBoard = updatedBoard }
  , score        = updatedScore
  , poweredTimer = updatedPwr
  }
  where
    lvl = level gs
    brd = gameBoard lvl
    plr = player gs
    scr = score gs
    pwr = poweredTimer gs
    playerPos = bimap floor floor (position plr)
    currTile = GB.get playerPos brd

    isAtPowerPellet = currTile == Just PowerPellet
    
    updatedPwr = if isAtPowerPellet 
      then pwr .+ 10000
      else pwr
    
    updatedBoard = case currTile of
      Just Pellet      -> GB.set playerPos Empty brd
      Just PowerPellet -> GB.set playerPos Empty brd
      _                -> brd

    updatedScore = case currTile of
      Just Pellet      -> scr .+ 10
      Just PowerPellet -> scr .+ 50
      _                -> scr