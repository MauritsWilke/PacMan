module Actions.Move where

import Model
import Utils.Board
import GHC.Num (integerFromInt)
import Data.Fixed (mod')
import Data.List
import Utils.PlayerUtil

playerMove :: GameState -> Direction -> Player
playerMove gs dir = (player gs) { position = pos, direction = dir'} where
  pos = case moveIsPossible gs (position (player gs)) 0.2 dir False of
    Nothing -> getPlayerPosition gs
    Just a  -> a
  dir' = if pos == getPlayerPosition gs then direction (player gs) else dir

-- check if provided argument is close enough to some n + 0.5 (for corner snap allowance)
closeEnough :: Float -> Bool
closeEnough v = abs (fromInteger (floor v) - v + 0.5) < allowedOffset
  where allowedOffset = 0.03

-- set fractional part of orthogonal direction to some n + 0.5
cornerSnap :: Direction -> Float -> Float -> (Float, Float)
cornerSnap dir x y
  | dir == North || dir == South = (x, fromInteger (floor y) + 0.5)
  | otherwise                    = (fromInteger (floor x) + 0.5, y)

-- checks to see if the desired move is allowed, 
-- returns Nothing if not and Just (Float,Float) 
-- if it is with the coordinates after the move
moveIsPossible :: GameState -> (Float, Float) -> PlayerSpeed -> Direction -> Bool -> Maybe (Float,Float)
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

-- setToMiddle :: (Float,Float) -> (Float,Float)
-- setToMiddle (row,col) = (fromInteger (floor row) + 0.5, fromInteger (floor col) + 0.5)

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

-- !TO DO: if at pellet -> set to empty tile and adjust score
-- TODO: GET RID OF fromMaybe !!!

-- ghost moves 
-- -> if not yet at destination, do a step towards destination 
-- -> if at destination, get new destination and step towards new destination 
ghostMove :: GameState -> Ghost -> Ghost
ghostMove gstate ghost =
    ghostStep gstate ghost bestDirection
    where
    bestDirection = if null allowedDirections
                      then oppositeDirection (ghostDirection ghost) -- if no allowed directions -> go back
                      else bestOf gstate ghost allowedDirections    -- else choose best direction

      -- check all legal directions except opposite
    allowedDirections = filter movableDirection $ delete (oppositeDirection (ghostDirection ghost)) allDirections

    -- checks if the provided direction is allowed
    movableDirection dir' =
      case moveIsPossible gstate (ghostPosition ghost) 0.2 dir' True of
         Nothing   -> False
         Just _    -> True -- setToMiddle' (ghostPosition ghost) (Just dir') /= ghostPosition ghost--a /= ghostPosition ghost
      -- where (x', y') = tileMove (ghostPosition ghost) dir'
      --       tile     = get (floor x', floor y') (gameBoard (level gstate))

-- set current location to middle of current tile or only the coordinate inline with the provided direction
setToMiddle :: (Float,Float) -> Maybe Direction -> (Float,Float)
setToMiddle (row,col) Nothing                                   = (fromInteger (floor row) + 0.5, fromInteger (floor col) + 0.5)
setToMiddle (row,col) (Just dir) | dir == North || dir == South = (fromInteger (floor row) + 0.5, col)
                                 | otherwise                    =  (row, fromInteger (floor col) + 0.5)

-- find best direction for move
-- IMPORTANT: doesn't check for validity, provided list should contain only valid directions
bestOf :: GameState -> Ghost -> [Direction] -> Direction
bestOf _ ghost []              = ghostDirection ghost -- oppositeDirection $-- if no valid directions, return opposite of current ghostdirection
bestOf gstate ghost directions = bestDirection                            -- else find the best direction
 where
  bestDirection = directions !! closestToGoal
  closestToGoal =
    case elemIndex (foldl1' min distances) distances of
      Nothing  -> 0
      Just num -> num -- minimum distances `elem` distances
  distances = map (distance ghostGoal . preMove currPosition 0.2) directions -- calculate all the distances of potential moves
  currPosition = ghostPosition ghost
  ghostGoal = goalAlgorithm gstate (ghostType ghost)

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
  where pos = moveIsPossible gstate (ghostPosition ghost) 0.2 dir True

-- return only the intermediate destination for which no re-evaluation is required 
-- (i.e. won't change overtime)
-- getDestination :: GameState -> Ghost -> (Float,Float)
-- getDestination gstate ghost = Actions.Move.traverse brd mps dir
--   where brd = gameBoard (level gstate)
--         mps = setToMiddle (ghostPosition ghost)
--         dir = ghostDirection ghost

-- keep moving until dillema (multiple possible directions (besides the one where the ghost came from)) 
-- traverse :: Board -> (Float,Float) -> Direction -> (Float,Float)
-- traverse b pos dir
--   -- we will re-evaluate later, for now this is the location that the ghost will move to (base case)
--   | length directionChoices /= 1 = pos
--   | otherwise                    = Actions.Move.traverse b nextPos nextDir
--   where
--     opposite  = oppositeDirection dir
--     nextPos   = tileMove pos dir
--     nextDir = head directionChoices

--     -- check all legal directions except opposite
--     directionChoices = filter allowedDirection $ delete opposite allDirections

--     -- checks if the provided direction is allowed (uses board)
--     allowedDirection dir' = case tile of
--         Nothing   -> False
--         Just Wall -> False
--         Just _    -> True
--       where (x', y') = tileMove pos dir'
--             tile     = get (floor x', floor y') b

-- get coordinates of next tile
tileMove :: (Float,Float) -> Direction -> (Float,Float)
tileMove (x, y) dir  = (x + xAdd, y + yAdd)
  where (xAdd, yAdd) = directionToTuple dir

-- getSpeed :: Bool -> RoundCounter -> Float
-- getSpeed isPlayer (RoundCounter roundIndex) = 

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection East  = West
oppositeDirection West  = East

-- in order of priority during scatter
allDirections :: [Direction]
allDirections = [North,West,South,East]

-- returns actual goal destination, based on state of the game and ghostType
goalAlgorithm :: GameState -> GhostType -> (Float,Float)
goalAlgorithm gstate Blinky = position $ player gstate -- direct chase
goalAlgorithm gstate Inky   = position $ player gstate -- relative to blinky and pac man
goalAlgorithm gstate Pinky  = position $ player gstate -- aim for 2 dots infront of pacman
goalAlgorithm gstate Clyde  = position $ player gstate -- direct chase, but scatter if within 8 dots of pacman

distance :: (Float,Float) -> (Float,Float) -> Float
distance (x,y) (a,b) = sqrt $ xOff * xOff + yOff * yOff
  where xOff = x-a
        yOff = y-b