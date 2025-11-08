module Main where

import Graphics.Gloss.Interface.IO.Game
import View
import Controller

import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Utils.Board (parseBoard)
import Model (NamedBoard(..), initialState, NamedSave (..))
import Data.Aeson (decodeFileStrict)
import Data.Maybe

main :: IO ()
main = do
  namedBoards <- loadBoards
  namedSaves <- loadSaves
  let st = initialState namedBoards namedSaves
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

loadSaves :: IO [NamedSave]
loadSaves = do
  files <- listDirectory "saves"
  let paths = map ("saves" </>) files
  saveStrings <- mapM decodeFileStrict paths

  let names = map takeBaseName files
      fltr = catMaybes saveStrings

  pure (zipWith NamedSave names fltr)