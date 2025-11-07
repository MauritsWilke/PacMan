{-# LANGUAGE RecordWildCards #-}
module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count
import Actions.Move

interact :: GameState -> GameState
interact gs = case scene gs of 
  SinglePlayer -> (interactPellets . autoMovePacman . reduceTimers) gs
  _            -> gs

autoMovePacman :: GameState -> GameState
autoMovePacman gs = gs { player = playerMove gs dir }
  where dir = (direction . player) gs

-- REDUCE ALL TIMERS BY 1, AUTO STOP AT 0
reduceTimers :: GameState -> GameState
reduceTimers gs = gs { level = updatedLevel }
  where updatedLevel = let lvl = level gs in lvl { ghosts = reduceGhostTimers (ghosts lvl) }

-- reduce ghostTimers
reduceGhostTimers :: [Ghost] -> [Ghost]
reduceGhostTimers [] = []
reduceGhostTimers (g@Ghost{..} : gs) 
 = g 
 { frightTimer  = frightTimer  .- 1
 , scatterTimer = scatterTimer .- 1
 , releaseTimer = releaseTimer .- 1
 } : reduceGhostTimers gs

interactPellets :: GameState -> GameState
interactPellets gs = action gs
  { level        = lvl { gameBoard = board' }
  , score        = score'
  , poweredTimer = poweredTimeCounter 10
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

    (score', action, ghs') = case tile of
      Just Pellet      -> (score gs .+ 10, id              , ghostsEaten gs)
      Just PowerPellet -> (score gs .+ 50, freightenGhosts , 0             )
      _                -> (score gs,       id              , ghostsEaten gs)
      

freightenGhosts :: GameState -> GameState
freightenGhosts gstate = gstate { level = newLevel }
  where
    lvl = level gstate
    newLevel = lvl { ghosts = map freighten ghostList }
    ghostList = ghosts lvl
    freighten ghost = ghost 
      { ghostDirection = oppositeDirection (ghostDirection ghost)
      , frightTimer    = frightTimeCounter 480 -- 8 sec * 60 fps
      } 