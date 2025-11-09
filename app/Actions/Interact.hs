{-# LANGUAGE RecordWildCards #-}
module Actions.Interact where

import Model as M
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count
import Actions.Move
import Utils.PlayerUtil
import View.Scenes.SelectBoard (exitScene)

interact :: GameState -> GameState
interact gs = case scene gs of
  SinglePlayer -> (checkGameOver . interactGhosts . awardExtraLives . interactPellets . playerMove . reduceTimers) gs
  _            -> gs

-- REDUCE ALL TIMERS BY 1, AUTO STOP AT 0
reduceTimers :: GameState -> GameState
reduceTimers gs = gs
  { level = updatedLevel
  , animation = animation' (animation gs)
  }
  where updatedLevel = let lvl = level gs in lvl { ghosts = reduceGhostTimers (ghosts lvl) }
        animation' (AnimationTimer i) = AnimationTimer ((i + 1)`mod` 24)

checkGameOver :: GameState -> GameState
checkGameOver gs = case lives gs of
  LiveCounter 0 -> gs { scene = GameOver }
  _             -> gs

awardExtraLives :: GameState -> GameState
awardExtraLives gs = gs 
  { lives = currentLives .+ toBeGranted
  , livesAwarded = alreadyawarded + toBeGranted 
  } where currentLives     = lives gs
          livesToBeAwarded = (getCount .score) gs `div` 10000
          alreadyawarded   = livesAwarded gs
          toBeGranted      = livesToBeAwarded - alreadyawarded

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
playerKilled gs = gs
  { lives  = lives' .- 1
  , level  = level'  { ghosts = resetGhosts gs ghosts' }
  , player = player' {position = getPlayerSpawn board', direction = East }
  } where player' = player gs
          board'  = gameBoard $ level gs
          lives'  = lives gs
          ghosts' = standardGhosts (gameBoard (level gs))
          level'  = level gs

resetGhosts :: GameState -> [Ghost] -> [Ghost]
resetGhosts gs = reset' where
    reset' []      = []
    reset' (g:ghs) = g
      { ghostPosition = getGhostSpawn (gameBoard (level gs)) (releaseIndex g)} : reset' ghs

interactGhosts  :: GameState -> GameState
interactGhosts  gs = case afterCollisionRes of -- check if pacman is eaten
  Nothing -> playerKilled gs                   -- if so, soft reset
  Just a  -> gs { level = lvl { ghosts = a } } -- if not, update ghosts
  where afterCollisionRes = afterCollision gs currGhostList
        currGhostList     = ghosts $ level gs
        lvl               = level gs

-- returns nothing if level requires soft reset, 
-- returns Just [Ghost] if pacman not eaten with the updated ghostlist
afterCollision :: GameState -> [Ghost] -> Maybe [Ghost]
afterCollision gs ghosts
  | any playerGetsEaten ghosts = Nothing
  | otherwise                  = Just (map update ghosts)
  where
    hit = eats (player gs)
    playerGetsEaten g = case hit g of -- ghost eats player
      Just False -> True   
      _          -> False

    update g = case hit g of -- player eats ghost
      Just True -> g 
                    { destination = Just destination
                    , ghostMode   = Spawn
                    , frightTimer = frightTimeCounter 0
                    }
      _         -> g
      where destination = getGhostSpawn (gameBoard (level gs)) (releaseIndex g)


interactPellets :: GameState -> GameState
interactPellets gs = checkLevelComplete $ action gs
  { level        = lvl { gameBoard = board' }
  , score        = score'
  -- , poweredTimer = poweredTimeCounter 10
  , ghostsEaten  = ghs'
  } where
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

checkLevelComplete :: GameState -> GameState
checkLevelComplete gs = if emptyBoard (gameBoard (level gs))
  then (exitScene gs) {lives = currLives, M.round = newRound, score = currScore}--gs {round = M.round gs .+ 1, level = lvl}
  else gs
  where currLives = lives gs
        newRound  = M.round gs .+ 1
        currScore = score gs



freightenGhosts :: GameState -> GameState
freightenGhosts gstate = gstate { level = newLevel }
  where
    lvl = level gstate
    newLevel = lvl { ghosts = map freighten ghostList }
    ghostList = ghosts lvl
    freighten ghost = ghost
      { ghostDirection = oppositeDirection (ghostDirection ghost)
      , frightTimer    = frightTimeCounter 480 -- 8 sec * 60 step function calls per second
      }