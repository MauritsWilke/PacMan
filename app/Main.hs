module Main where

import Graphics.Gloss.Interface.IO.Game
import View
import Controller

import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Utils.Board (parseBoard)
import Model (NamedBoard(..), initialState)

main :: IO ()
main = do
  namedBoards <- loadBoards
  let st = initialState namedBoards
  playIO (InWindow "Pac-Man" (400, 400) (0, 0))
    black
    60
    st
    view
    input
    step

loadBoards :: IO [NamedBoard]
loadBoards = do
  files <- listDirectory "boards"
  let paths = map ("boards" </>) files
  boardStrings <- mapM readFile paths

  let names  = map takeBaseName files
      boards = map parseBoard boardStrings 

  pure (zipWith NamedBoard names boards)
