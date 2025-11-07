{-# LANGUAGE RecordWildCards #-}
module Actions.Move where

import Model
import Utils.Board
import GHC.Num (integerFromInt)
import Data.Fixed (mod')
import Data.List
import Utils.Count

updatePlayerDir :: GameState -> Direction -> GameState
updatePlayerDir gs dir = gs { player = plr { direction = newDir, queuedDir = dir } }
  where
    plr = player gs
    dirIsAllowed = moveIsPossible gs (position plr) (playerSpeed gs) dir False
    newDir = case dirIsAllowed of
      Just _  -> dir
      Nothing -> direction plr


playerMove :: GameState -> GameState
playerMove gs = gs { player = plr' }
  where 
    plr = player gs
    dir = direction plr
    que = queuedDir plr
    pos = position plr
    movQue = moveIsPossible gs pos (playerSpeed gs) que False
    movDir = moveIsPossible gs pos (playerSpeed gs) dir False

    plr' = case movQue of
      Just a  -> plr { direction = que, position = a }
      Nothing -> case movDir of
        Just a  -> plr { position = a }
        Nothing -> plr

-- check if provided argument is close enough to some n + 0.5 (for corner snap allowance)
closeEnough :: Float -> Bool
closeEnough v = abs (fromInteger (floor v) - v + 0.5) < allowedOffset
  where allowedOffset = 0.08

-- set fractional part of orthogonal direction to some n + 0.5
cornerSnap :: Direction -> Float -> Float -> (Float, Float)
cornerSnap dir x y
  | dir == North || dir == South = (x, fromInteger (floor y) + 0.5)
  | otherwise                    = (fromInteger (floor x) + 0.5, y)

-- checks to see if the desired move is allowed, 
-- returns Nothing if not and Just (Float,Float) 
-- if it is with the coordinates after the move
moveIsPossible :: GameState -> (Float, Float) -> Speed -> Direction -> Bool -> Maybe (Float,Float)
moveIsPossible gs (x,y) speed dir isGhost = let
  (xOff, yOff)         = directionToTuple dir
  (desiredX, desiredY) = (x + xOff * speed, y + yOff * speed)
  tileToCheck          = getTileToCheck (desiredX,desiredY) dir
  brd                  = gameBoard $ level gs
  isCloseEnough        = if dir == North || dir == South then closeEnough y else closeEnough x
  in if isCloseEnough then case get tileToCheck brd of
   Nothing        -> getWrapAround (desiredX,desiredY) brd -- check for wrap-around if edge of map
   Just Wall      -> if (x,y) == setToMiddle (x,y) (Just dir) then Nothing else Just $ setToMiddle (x,y) Nothing  -- set to end of allyway if not yet exact
   Just GhostExit -> if isGhost then Just (cornerSnap dir desiredX desiredY) else Just $ setToMiddle (x,y) Nothing -- set to end of allyway if a regular player, otherwise do a normal move
   Just _         -> Just (cornerSnap dir desiredX desiredY) -- if move possible -> update offset of direction and set other direction to some n + 0.5 (to allign with middle)
  else Nothing -- if to far from edge -> check if move still possible if not, stay in place

getWrapAround :: (Float, Float) -> Board -> Maybe (Float, Float)
getWrapAround (x,y) b
  | onEdge && accesibleOtherSide = Just otherSide -- if the other side of the board has an accesible slot -> return other side
  | otherwise = Nothing
  where
    onEdge = x <= 0.5
          || y <= 0.5
          || ceiling (x - 0.499) >= integerFromInt (height b) -- check if in or over middle of tile at the edge of the board
          || ceiling (y - 0.499) >= integerFromInt (width  b)
    accesibleOtherSide = -- only true when not a wall | ghostexit | ghostspawn
      case get (floor otherX, floor otherY) b of
       Nothing         -> error "board size seems to be inaccurate"
       Just Wall       -> False
       Just GhostExit  -> False
       Just GhostSpawn -> False
       Just _          -> True
    otherSide@(otherX, otherY) = (mod' x (fromIntegral (height b)), mod' y (fromIntegral (width b)))

directionToTuple :: Direction -> (Float,Float)
directionToTuple North = (1 , 0)
directionToTuple South = (-1, 0)
directionToTuple East  = (0 , 1)
directionToTuple West  = (0 ,-1)

-- based on position and direction, find out which tile pacman is trying to move towards
getTileToCheck :: (Float,Float) -> Direction -> TileCoordinates
getTileToCheck (x,y) dir
  | dir == North || dir == South = (floor (x + offset), floor y)
  | otherwise                    = (floor x, floor (y + offset))
 where offset = if dir == North || dir == East then 0.5 else (-0.5)

-- ghost moves 
-- -> do a move based on gamestate and ghost input
ghostMove :: GameState -> Ghost -> Ghost
ghostMove gstate ghost = 
    if getCount (frightTimer ghost) > 0 then ghost else ghostStep gstate ghost bestDirection
    where
    bestDirection = if null allowedDirections
          then oppositeDirection (ghostDirection ghost) -- if no allowed directions -> go back
          else bestOf gstate ghost allowedDirections (getCount (Model.scatterTimer ghost) > 0)   -- else choose best direction

    -- check all legal directions except opposite
    allowedDirections = filter movableDirection $ delete (oppositeDirection (ghostDirection ghost)) allDirections

    -- checks if the provided direction is allowed
    movableDirection dir' =
      case moveIsPossible gstate (ghostPosition ghost) (ghostSpeed gstate) dir' True of
         Nothing   -> False
         Just _    -> True

-- set current location to middle of current tile or only the coordinate inline with the provided direction
setToMiddle :: (Float,Float) -> Maybe Direction -> (Float,Float)
setToMiddle (row,col) Nothing                                   = (fromInteger (floor row) + 0.5, fromInteger (floor col) + 0.5)
setToMiddle (row,col) (Just dir) | dir == North || dir == South = (fromInteger (floor row) + 0.5, col)
                                 | otherwise                    =  (row, fromInteger (floor col) + 0.5)

-- find best direction for move
-- IMPORTANT: doesn't check for validity, provided list should contain only valid directions
bestOf :: GameState -> Ghost -> [Direction] -> Bool -> Direction
bestOf _ ghost [] _                       = ghostDirection ghost -- if no valid directions, return opposite of current ghostdirection
bestOf gstate ghost directions scattering = bestDirection        -- else find the best direction
 where
  bestDirection = directions !! closestToGoal
  closestToGoal =
    case elemIndex (foldl1' min distances) distances of
      Nothing  -> 0
      Just num -> num -- minimum distances `elem` distances
  distances = map (distance ghostGoal . preMove currPosition (ghostSpeed gstate)) directions -- calculate all the distances of potential moves
  currPosition = ghostPosition ghost
  ghostGoal = if scattering then scatter gstate ghost else goalAlgorithm gstate ghost

preMove :: (Float,Float) -> Float -> Direction -> (Float,Float)
preMove (x,y) speed dir = (x+speed*xOff ,y+speed*yOff)
  where (xOff,yOff) = directionToTuple dir

-- should have correct direction -> will execute the next move
ghostStep :: GameState -> Ghost -> Direction -> Ghost
ghostStep gstate ghost dir =
  case pos of
    Nothing -> ghost
    Just a  -> ghost {ghostPosition = a, ghostDirection = dir} --if a == setToMiddle' (ghostPosition ghost)  (Just dir) then ghost else 
  -- ghost { ghostPosition = pos, ghostDirection = dir}
  where pos = moveIsPossible gstate (ghostPosition ghost) (ghostSpeed gstate) dir True

-- get coordinates of next tile
tileMove :: (Float,Float) -> Direction -> (Float,Float)
tileMove (x, y) dir  = (x + xAdd, y + yAdd)
  where (xAdd, yAdd) = directionToTuple dir

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection East  = West
oppositeDirection West  = East

-- in order of priority during scatter
allDirections :: [Direction]
allDirections = [North,West,South,East]

-- returns actual goal destination, based on state of the game and ghostType
goalAlgorithm :: GameState -> Ghost -> (Float,Float)
goalAlgorithm gstate Ghost{..} = 
  case ghostType of 
    Blinky -> position $ player gstate  -- direct chase
    Inky   -> inky gstate ghostPosition -- relative to blinky and pac man
    Pinky  -> twoInFrontPacman gstate  -- aim for 2 dots infront of pacman
    Clyde  -> if distance ghostPosition (position $ player gstate) < 8 -- direct chase, but scatter if within 8 dots of pacman
              then bottomLeft $ gameBoard $ level gstate
              else twoInFrontPacman gstate 

scatter :: GameState -> Ghost -> (Float,Float)
scatter gstate Ghost{..} = 
  case ghostType of 
    Blinky -> topRight $ gameBoard $ level gstate  -- top-right corner
    Inky   -> bottomRight $ gameBoard $ level gstate -- bottom-right corner
    Pinky  -> topLeft $ gameBoard $ level gstate  -- top-left corner
    Clyde  -> bottomLeft $ gameBoard $ level gstate -- bottom-left corner

-- base goal tile off of pacman & first Blinky ghost in list, if no Blinky in list -> use provided ghost-location instead
inky :: GameState -> (Float,Float) -> (Float, Float) 
inky gstate (x,y) = (refX + 2*xOff, refY + 2*yOff)
 where (xOff,yOff) = (pacX-refX,pacY-refY)
       (pacX, pacY) = twoInFrontPacman gstate
       (refX,refY) = if not (null allBlinkies) then ghostPosition $ head allBlinkies else (x,y)
       allBlinkies = filter (isBlinky . ghostType) ghostList
       ghostList = ghosts $ level gstate
       isBlinky g = case g of 
        Blinky -> True
        _ -> False 

-- get the index of pacman + 2 times its direction (to predict movement)
twoInFrontPacman :: GameState -> (Float,Float)
twoInFrontPacman gstate = (x+2*xOff,y+2*yOff)
  where (x,y)       = position $ player gstate
        (xOff,yOff) = directionToTuple $ direction $ player gstate

distance :: (Float,Float) -> (Float,Float) -> Float
distance (x,y) (a,b) = sqrt $ xOff * xOff + yOff * yOff
  where xOff = x-a
        yOff = y-b