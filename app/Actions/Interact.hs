module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count

interact :: GameState -> GameState
interact = interactPellets . reduceTimers

-- REDUCE ALL TIMERS BY 1, AUTO STOP AT 0
reduceTimers :: GameState -> GameState
reduceTimers gs = gs

interactPellets :: GameState -> GameState
interactPellets gs = gs
  { level        = lvl { gameBoard = board' }
  , score        = score'
  , poweredTimer = pwr'
  , ghostsEaten  = ghs'
  }
  where
    lvl       = level gs
    brd       = gameBoard lvl
    plr       = player gs
    pos       = bimap floor floor (position plr)
    tile      = GB.get pos brd

    removePellet = maybe brd (const (GB.set pos Empty brd)) tile
    board'       = case tile of
                     Just Pellet      -> removePellet
                     Just PowerPellet -> removePellet
                     _                -> brd

    (score', pwr', ghs') = case tile of
      Just Pellet      -> (score gs .+ 10, poweredTimer gs         , ghostsEaten gs)
      Just PowerPellet -> (score gs .+ 50, poweredTimer gs .+ 10000, 0             )
      _                -> (score gs,       poweredTimer gs         , ghostsEaten gs)