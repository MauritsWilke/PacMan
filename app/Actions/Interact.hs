{-# LANGUAGE RecordWildCards #-}
module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count
import Actions.Move
import Utils.PlayerUtil

interact :: GameState -> GameState
interact gs = case scene gs of
  SinglePlayer -> (interactGhosts . interactPellets . autoMovePacman . reduceTimers) gs
  _            -> gs

autoMovePacman :: GameState -> GameState
autoMovePacman = playerMove

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

playerKilled :: GameState -> GameState
playerKilled gs = gs {lives = lives' .- 1, level = level' {ghosts = resetGhosts gs ghosts'}, player = player' {position = getPlayerSpawn board'}}
  where player' = player gs
        board'  = gameBoard $ level gs
        lives'  = lives gs
        ghosts' = ghosts $ level gs
        level'  = level gs

resetGhosts :: GameState -> [Ghost] -> [Ghost]
resetGhosts gs = reset' 0
  where reset' _ []    = []
        reset' i (g:ghs) = g {ghostPosition = getGhostSpawn (gameBoard (level gs)) i} : reset' (i+1) ghs

interactGhosts  :: GameState -> GameState
interactGhosts  gs = case afterCollisionRes of -- check if pacman is eaten
  Nothing -> playerKilled gs                   -- if so, soft reset
  Just a  -> gs {level = lvl {ghosts = a}}     -- if not, update ghosts
  where afterCollisionRes = afterCollision gs currGhostList
        currGhostList     = ghosts $ level gs
        lvl               = level gs

-- returns nothing if level requires soft reset, returns Just [Ghost] if pacman not eaten with the updated ghostlist
afterCollision :: GameState -> [Ghost] -> Maybe [Ghost]
afterCollision gs ghs  | reset     = Nothing
                       | otherwise = Just newGhosts
  where reset          = any needsReset ghs -- check if any ghost eats pacman
        needsReset g'  = case hitCheckResult g' of
          Nothing -> False
          Just True -> False
          Just False -> True
        hitCheckResult = eats (player gs)
        newGhosts      = map updateGhost ghs
        updateGhost g'  = case eats (player gs) g' of
          Nothing    -> g' -- if ghost doesn't hit player, nothing happens
          Just True  -> g' {respawning = Just (getGhostSpawn (gameBoard (level gs)) 0), ghostMode = Spawn}   -- if player eats ghost, ghost return to spawn
          Just False -> error "this should be covered by reset check"

interactPellets :: GameState -> GameState
interactPellets gs = action gs
  { level        = lvl { gameBoard = board' }
  , score        = score'
  -- , poweredTimer = poweredTimeCounter 10
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