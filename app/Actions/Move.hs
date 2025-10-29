module Actions.Move where
  
import Model
import Utils.Board
import GHC.Num (integerFromInt)
import Data.Fixed (mod')

move :: GameState -> Direction -> Player
move gs dir = (player gs) { position = pos } where
  pos = case moveIsPossible gs dir of
    Nothing -> getPlayerPosition gs
    Just a -> a

closeEnough :: Float -> Bool
closeEnough v = abs (fromInteger (floor v) - v + 0.5) < allowedOffset
  where allowedOffset = 0.03

cornerSnap :: Direction -> Float -> Float -> (Float, Float)
cornerSnap dir x y 
  | dir == North || dir == South = (x, fromInteger (floor y) + 0.5)
  | otherwise                    = (fromInteger (floor x) + 0.5, y)

moveIsPossible :: GameState -> Direction -> Maybe (Float,Float)
moveIsPossible gs dir = let
  (x, y)               = getPlayerPosition gs
  (xOff, yOff)         = directionToTuple dir
  (desiredX, desiredY) = (x + xOff * 0.2, y + yOff * 0.2)
  tileToCheck          = getTileToCheck (desiredX,desiredY) dir
  b = gameBoard $ level gs
  isCloseEnough = if dir == North || dir == South then closeEnough y else closeEnough x
  in if isCloseEnough then case get tileToCheck b of
   Nothing        -> getWrapAround (desiredX,desiredY) b     -- check for wrap-around if edge of map
   Just Wall      -> Just $ setToMiddle (x,y)                -- set to end of allyway
   Just GhostExit -> Just $ setToMiddle (x,y)                -- set to en of allyway
   Just _         -> Just (cornerSnap dir desiredX desiredY) -- if move possible -> update offset of direction and set other direction to some n + 0.5 (to allign with middle)
  else Nothing --getWrapAround (x,y) b -- if to far from edge -> stay in place

getWrapAround :: (Float, Float) -> Board -> Maybe (Float, Float)
getWrapAround (x,y) b 
  | onEdge && accesibleOtherSide = Just otherSide -- if the other side of the board has an accesible slot -> return other side
  | otherwise = Nothing
  where
    onEdge = x <= 0.5 
          || y <= 0.5 
          || ceiling (x - 0.499) >= integerFromInt (height b) -- check if in or over middle of tile near at edge
          || ceiling (y - 0.499) >= integerFromInt (width  b)
    accesibleOtherSide = -- only true when not a wall | ghostexit | ghostspawn
      case get (floor otherX, floor otherY) b of
       Nothing         -> error "board size seems to be inaccurate"
       Just Wall       -> False
       Just GhostExit  -> False
       Just GhostSpawn -> False
       Just _          -> True
    otherSide@(otherX, otherY) = (mod' x (fromIntegral (height b)), mod' y (fromIntegral (width b)))

setToMiddle :: (Float,Float) -> (Float,Float)
setToMiddle (x,y) = (fromInteger (floor x) + 0.5, fromInteger (floor y) + 0.5)

directionToTuple :: Direction -> (Float,Float)
directionToTuple North = (1 , 0)
directionToTuple South = (-1, 0)
directionToTuple East  = (0 , 1)
directionToTuple West  = (0 ,-1)

-- based on position and direction, find out which tile pacman is trying to move to
getTileToCheck :: (Float,Float) -> Direction -> (Int,Int)
getTileToCheck (x,y) dir 
  | dir == North || dir == South = (floor (x + offset), floor y)
  | otherwise                    = (floor x, floor (y + offset))
 where offset = if dir == North || dir == East then 0.5 else (-0.5)

getPlayerPosition :: GameState -> (Float, Float)
getPlayerPosition = position . player