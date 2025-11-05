module Utils.Board where
import qualified Data.IntMap.Lazy as I
import Model

set :: TileCoordinates -> Tile -> Board -> Board
set (row,col) t (Board m w h) = Board (I.adjustWithKey f ((row * w) + col) m) w h
    where f _ _ = t

get :: TileCoordinates -> Board -> Maybe Tile
get (row,col) (Board m w h) 
  | validIndex = I.lookup ((row * w) + col) m
  | otherwise  = Nothing
 where validIndex = (row < h && row >= 0) && (col < w && col >= 0)
             
indexToCoord :: Int -> Int -> (Int, Int)
indexToCoord i w = (i `div` w, i `mod` w)

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

{-
Orientation = N | E | S | W

DoubleCorner
DoubleWall
DoubleWall

SingleCorner
SingleWall
-}