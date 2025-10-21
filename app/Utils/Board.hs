module Utils.Board where
import qualified Data.IntMap.Lazy as I

data Board = Board 
  { board :: I.IntMap Tile
  -- , wallMap :: I.IntMap Wall
  , width :: Int
  , height:: Int
  } deriving (Show)

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
    coordList = [0..99]
    emptyTiles = replicate (length coordList) Pellet

realBoard :: Board
realBoard = Board {
  board = I.fromList boardList,
  width = w,
  height = h
}
  where
    boardList = zip coordList tiles
    coordList = [0..((w * h) - 1)]
    w = 28
    h = 31
    tiles = [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,PowerPellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,PowerPellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Pellet,Empty,Empty,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Empty,Empty,Pellet,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,GhostExit,GhostExit,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,PowerPellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,PowerPellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]

charToTile :: Char -> Tile
charToTile 'W' = Wall
charToTile 'E' = Empty
charToTile 'P' = Pellet
charToTile 'S' = PowerPellet
charToTile 'F' = Fruit
charToTile 'G' = GhostSpawn
charToTile 'H' = GhostExit
charToTile _   = Empty

loadBoardFromFile :: IO [Tile]
loadBoardFromFile = do
  contents <- readFile "boards/pacman"
  pure . map (charToTile . head) . concatMap words . reverse . lines $ contents

data Tile = Wall | Empty | Pellet | PowerPellet | Fruit | GhostSpawn | GhostExit
  deriving (Show, Eq)

{-
Orientation = N | E | S | W

DoubleCorner
DoubleWall
DoubleWall

SingleCorner
SingleWall
-}