module Utils.Board where
import qualified Data.IntMap.Lazy as I

data Board = Board {board :: I.IntMap Tile, width :: Int, height:: Int}
  deriving (Show)

set :: (Int,Int) -> Tile -> Board -> Board
set (row,col) t (Board m w h) = Board (I.adjustWithKey f (row*col) m) w h
    where f _ _ = t

get :: (Int,Int) -> Board -> Maybe Tile
get (row,col) (Board m _ _) = I.lookup (row*col) m

-- default Tile = Wall -> creates a w*h IntMap Wall after inserting elements of provided list 
-- for each ((row,col),tile) <- 
-- customBoard :: [((Int,Int),Tile)] -> Int -> Int -> Board
-- customBoard ls w h = Board (foldl (set . fst) wallMap (map snd ls)) w h
--   where wallMap = I.fromList (zip [1..(w*h)] (replicate (w*h) Wall))

standardBoard :: Board
standardBoard = Board {board = I.fromList boardList, width = 10, height = 10}
  where
    boardList = zip coordList emptyTiles
    coordList = [1..100]
    emptyTiles = replicate (length coordList) Pellet

data Tile = Wall | Empty | Pellet | PowerPellet | Fruit
  deriving (Show, Eq)


