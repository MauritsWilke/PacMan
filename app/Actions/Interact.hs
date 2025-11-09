{-# LANGUAGE RecordWildCards #-}
module Actions.Interact where

import Model as M
import Utils.Board as GB
import Data.Bifunctor (bimap)
import Utils.Count
import Actions.Move
import Utils.PlayerUtil
import View.Scenes.SelectBoard (exitScene)
import Data.Function ((&))

interact :: GameState -> GameState
interact gs = case scene gs of
  SinglePlayer -> gs
                & reduceTimers
                & playerMove
                & interactPellets
                & interactGhosts
                & awardExtraLives
                & checkGameOver
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
          livesToBeAwarded = (getCount . score) gs `div` 10000
          alreadyawarded   = livesAwarded gs
          toBeGranted      = livesToBeAwarded - alreadyawarded

-- reduce ghostTimers
reduceGhostTimers :: [Ghost] -> [Ghost]
reduceGhostTimers [] = []
reduceGhostTimers (g@Ghost{..} : gs)
 = g
 { frightTimer  = frightTimer  .- 1
 , scatterTimer = if getCount scatterTimer == 0 then ScatterTimer resetTimer else scatterTimer .- 1
 , releaseTimer = releaseTimer .- 1
 , ghostMode    = ghostMode'
 } : reduceGhostTimers gs
 where
  resetTimer = case ghostMode' of
    Chase   -> 20 * 60 -- 20 seconds * 60 frames per second
    Scatter -> 7  * 60 -- 7  seconds * 60 frames per second 
    _       -> 0

  ghostMode' = if getCount scatterTimer == 0 then switch ghostMode else ghostMode
  switch m   = case m of
    Chase   -> Scatter
    Scatter -> Chase
    x       -> x

playerKilled :: GameState -> GameState
playerKilled gs = gs
  { lives  = lives' .- 1
  , level  = level'  { ghosts = resetGhosts gs ghosts' }
  , player = player' { position = getPlayerSpawn board', direction = East }
  } where player' = player gs
          board'  = gameBoard $ level gs
          lives'  = lives gs
          ghosts' = standardGhosts (gameBoard (level gs))
          level'  = level gs

resetGhosts :: GameState -> [Ghost] -> [Ghost]
resetGhosts gs = map reset' where
    reset' g = g
      { ghostPosition = getGhostSpawn (gameBoard (level gs)) (releaseIndex g)}

interactGhosts  :: GameState -> GameState
interactGhosts  gs =
  case afterCollisionRes of -- check if pacman is eaten
    Nothing    -> playerKilled gs  -- if so, soft reset
    Just (s,a) -> let extraPoints = if s > 0 then ghostPoints (ghostsEaten gs + s) else 0 in gs
      { score       = score gs .+ extraPoints
      , ghostsEaten = ghostsEaten gs +  s
      , level       = lvl { ghosts = a }
      } -- if not, update ghosts and score
  where afterCollisionRes = afterCollision gs currGhostList
        currGhostList     = ghosts $ level gs
        lvl               = level gs

-- returns nothing if level requires soft reset, 
-- returns Just (eatenGhosts, [Ghost]) if pacman not eaten with the amount of extra eaten ghosts and updated ghostlist
afterCollision :: GameState -> [Ghost] -> Maybe (Int, [Ghost])
afterCollision gs ghosts
  | any playerGetsEaten ghosts = Nothing
  | otherwise                  = Just (sum (map fst updatedGhosts) , map snd updatedGhosts)
  where
    updatedGhosts = map update ghosts
    hit = eats (player gs)
    playerGetsEaten g = case hit g of -- ghost eats player
      Just False -> True
      _          -> False

    update g = case hit g of -- player eats ghost
      Just True -> (1, g
                    { destination = Just destination
                    , ghostMode   = Spawn
                    , frightTimer = frightTimeCounter 0
                    })
      _         -> (0, g)
      where destination = getGhostSpawn (gameBoard (level gs)) (releaseIndex g)

interactPellets :: GameState -> GameState
interactPellets gs = checkLevelComplete $ action gs
  { level        = lvl { gameBoard = board' }
  , score        = score'
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
      Just Pellet      -> (score gs .+ 10, id       , ghostsEaten gs)
      Just PowerPellet -> (score gs .+ 50, transform, 0             )
      _                -> (score gs      , id       , ghostsEaten gs)

    transform            = resetGhostsEaten . freightenGhosts
    resetGhostsEaten gs' = gs' {ghostsEaten = 0}

checkLevelComplete :: GameState -> GameState
checkLevelComplete gs = if emptyBoard b
  then (exitScene gs)
    { player  = player'
    , lives   = currLives
    , M.round = newRound
    , score   = currScore }
  else gs
  where currLives = lives gs
        newRound  = M.round gs .+ 1
        currScore = score gs
        player'   = (player gs) { position = getPlayerSpawn b, direction = East }
        b = gameBoard $ level gs

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