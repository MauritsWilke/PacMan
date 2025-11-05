module Actions.Move where

import Model
import Utils.Board
import GHC.Num (integerFromInt)
import Data.Fixed (mod')
import Data.Maybe (isJust, fromMaybe)
import Data.List
import Utils.PlayerUtil

playerMove :: GameState -> Direction -> Player
playerMove gs dir = (player gs) { position = pos } where
  pos = case moveIsPossible gs (position (player gs)) 0.2 dir False of
    Nothing -> getPlayerPosition gs
    Just a  -> a

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
  b                    = gameBoard $ level gs
  isCloseEnough        = if dir == North || dir == South then closeEnough y else closeEnough x
  in if isCloseEnough then case get tileToCheck b of
   Nothing        -> getWrapAround (desiredX,desiredY) b        -- check for wrap-around if edge of map
   Just Wall      -> Just $ setToMiddle (x,y)           -- set to end of allyway
   Just GhostExit -> if isGhost then Just (cornerSnap dir desiredX desiredY) else Just $ setToMiddle (x,y) -- set to end of allyway if a regular player, otherwise do a normal move
   Just _         -> Just (cornerSnap dir desiredX desiredY) -- if move possible -> update offset of direction and set other direction to some n + 0.5 (to allign with middle)
  else Nothing -- if to far from edge -> check if move still possible if not, stay in place

-- since pacman is as large as a tile, it will atmost enter 2 new tiles on a move, if both are valid, the move is valid
-- checkOtherOverlappingTile :: Board -> (Float,Float) -> Direction -> Maybe (Float,Float)
-- checkOtherOverlappingTile b (row,col) dir = Nothing
--   where (x,y) = 
--         valid = get tileToCheck b

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

-- set current location to middle of current tile or only the coordinate inline with the provided direction
setToMiddle' :: (Float,Float) -> Maybe Direction -> (Float,Float)
setToMiddle' (row,col) Nothing                                   = (fromInteger (floor row) + 0.5, fromInteger (floor col) + 0.5)
setToMiddle' (row,col) (Just dir) | dir == North || dir == South = (fromInteger (floor row) + 0.5, col)
                                 | otherwise                    =  (row, fromInteger (floor col) + 0.5)

setToMiddle :: (Float,Float) -> (Float,Float)
setToMiddle (row,col) = (fromInteger (floor row) + 0.5, fromInteger (floor col) + 0.5)

directionToTuple :: Direction -> (Float,Float)
directionToTuple North = (1 , 0)
directionToTuple South = (-1, 0)
directionToTuple East  = (0 , 1)
directionToTuple West  = (0 ,-1)

-- based on position and direction, find out which tile pacman is trying to move to
getTileToCheck :: (Float,Float) -> Direction -> TileCoordinates
getTileToCheck (x,y) dir
  | dir == North || dir == South = (floor (x + offset), floor y)
  | otherwise                    = (floor x, floor (y + offset))
 where offset = if dir == North || dir == East then 0.5 else (-0.5)

-- !TO DO: if at pellet -> set to empty tile and adjust score
-- TODO: GET RID OF fromMaybe !!!

-- ghost moves -> if not yet at destination
-- Important: Do not change ghosts in game state directly
-- game state is only for info about surroundings -> return type 
ghostMove :: GameState -> Ghost -> Ghost
ghostMove gstate ghost
  | hasDestination = ghostStep gstate $ if atDestination ghost
    then ghost { destination = Just (getDestination gstate ghost) }
     --if not yet at destination -> move towards it | otherwise update destination & move
    else ghostStep gstate ghost
  | otherwise = ghostStep gstate ghost -- if no destination -> update destination & move
  where hasDestination       = isJust (destination ghost)
        -- check if both x and y close enough to destination
        -- parse destination to its value, if no destination -> return impossible coordinate
        atDestination ghost' =
          let des             = fromMaybe (-1,-1) (destination ghost')
              pos@(xPos,yPos) = ghostPosition ghost' in
          setToMiddle des == setToMiddle pos
                          && (closeEnough xPos && closeEnough yPos || False)

-- should have correct destination -> will execute the next move
ghostStep :: GameState -> Ghost -> Ghost
ghostStep = undefined

-- return only the intermediate destination for which no re-evaluation is required 
-- (i.e. won't change overtime)
getDestination :: GameState -> Ghost -> (Float,Float)
getDestination gstate ghost = Actions.Move.traverse brd mps dir
  where brd = gameBoard (level gstate)
        mps = setToMiddle (ghostPosition ghost)
        dir = ghostDirection ghost

-- keep moving until dillema (multiple possible directions (besides the one where the ghost came from)) 
traverse :: Board -> (Float,Float) -> Direction -> (Float,Float)
traverse b pos dir
  -- we will re-evaluate later, for now this is the location that the ghost will move to (base case)
  | length directionChoices /= 1 = pos
  | otherwise                    = Actions.Move.traverse b nextPos nextDir
  where
    opposite  = oppositeDirection dir
    nextPos   = tileMove pos dir
    nextDir = head directionChoices

    -- check all legal directions except opposite
    directionChoices = filter allowedDirection $ delete opposite allDirections 

    -- checks if the provided direction is allowed (uses board)
    allowedDirection dir' = case tile of
        Nothing   -> False
        Just Wall -> False
        Just _    -> True
      where (x', y') = tileMove pos dir'
            tile     = get (floor x', floor y') b

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

-- returns actual goal destination, which might change overtime
goalAlgorithm :: GameState -> GhostType -> (Float,Float)
goalAlgorithm gstate Blinky = position $ player gstate -- direct chase
goalAlgorithm gstate Inky   = position $ player gstate -- relative to blinky and pac man
goalAlgorithm gstate Pinky  = position $ player gstate -- aim for 2 dots infront of pacman
goalAlgorithm gstate Clyde  = position $ player gstate -- direct chase, but scatter if within 8 dots of pacman