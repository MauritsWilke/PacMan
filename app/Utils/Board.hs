{-# LANGUAGE RecordWildCards #-}
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

charToTile :: Char -> Tile
charToTile 'W' = Wall
charToTile 'E' = Empty
charToTile 'P' = Pellet
charToTile 'S' = PowerPellet
charToTile 'F' = Fruit
charToTile 'G' = GhostSpawn
charToTile 'H' = GhostExit
charToTile 'X' = PlayerSpawn
charToTile _   = Empty

parseBoard :: String -> Board
parseBoard contents =
  let rows        = lines contents
      boardHeight = length rows
      boardWidth  = length (words (head rows))
      tiles       = Prelude.map (charToTile . head) . concatMap words . reverse $ rows
      coords      = [0 .. length tiles - 1]
      boardList   = zip coords tiles
  in Board (I.fromList boardList) boardWidth boardHeight

-- get the indices of the corners of the board
topLeft :: Board -> (Float,Float)
topLeft Board{..}    = (fromIntegral height,0)

topRight :: Board -> (Float,Float)
topRight Board{..}    = (fromIntegral height,fromIntegral width) 

bottomLeft :: Board -> (Float,Float)
bottomLeft _ = (0,0) 

bottomRight :: Board -> (Float,Float)
bottomRight Board{..} = (0,fromIntegral width) 
