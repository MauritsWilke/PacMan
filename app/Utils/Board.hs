{-# LANGUAGE RecordWildCards #-}
module Utils.Board where
import qualified Data.IntMap.Lazy as I
import Model
import Data.List
import Data.Char (isSpace)

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
charToTile 'G' = GhostSpawn
charToTile 'H' = GhostExit
charToTile 'X' = PlayerSpawn
charToTile _   = Empty

sameLengths :: [[a]] -> Bool
sameLengths [] = True
sameLengths (x:xs) = all (\y -> length y == length x) xs

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

verifyBoard :: String -> Bool
verifyBoard contents = validRows && hasGhostSpawn && hasGhostExit && hasPlayerSpawn && hasOnePellet
  where
    rows = map trim (lines contents)
    validRows = sameLengths rows -- indirectly also validates columns

    hasGhostSpawn  = 'G' `elem` contents -- trivial
    hasGhostExit   = 'H' `elem` contents -- tivial
    hasPlayerSpawn = 'X' `elem` contents -- trivial
    hasOnePellet   = 'P' `elem` contents -- else you always win the level

parseBoard :: String -> Maybe Board
parseBoard contents = if verifyBoard contents then
  let 
    rows        = lines contents
    boardHeight = length rows
    boardWidth  = length (words (head rows))
    tiles       = Prelude.map (charToTile . head) . concatMap words . reverse $ rows
    coords      = [0 .. length tiles - 1]
    boardList   = zip coords tiles
  in   Just (Board (I.fromList boardList) boardWidth boardHeight)
  else Nothing

-- get the indices of the corners of the board
topLeft :: Board -> (Float,Float)
topLeft Board{..} = (fromIntegral height,0)

topRight :: Board -> (Float,Float)
topRight Board{..} = (fromIntegral height,fromIntegral width) 

bottomLeft :: Board -> (Float,Float)
bottomLeft _ = (0,0) 

bottomRight :: Board -> (Float,Float)
bottomRight Board{..} = (0,fromIntegral width) 
