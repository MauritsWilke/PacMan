module Main where

import Graphics.Gloss.Interface.IO.Game
import View
import Controller

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath  ((</>), takeBaseName)
import Utils.Board (parseBoard)
import Model (NamedBoard(..), initialState, NamedSave (..))
import Data.Aeson (decodeFileStrict)
import Control.Monad (filterM)
import Data.Traversable (forM)

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

-- | Load boards from a local directory
loadBoards :: IO [NamedBoard]
loadBoards = do
  let dir = "boards"
  exists <- doesDirectoryExist dir -- Prevent crashes when no directory is found
  if not exists
    then pure []
    else do
      files <- listDirectory dir

      let fullPaths = map (dir </>) files
      realFiles <- filterM doesFileExist fullPaths

      parsed <- forM realFiles $ \fp -> do
        content <- readFile fp
        pure (takeBaseName fp, parseBoard content)

      let good = [(name, board) | (name, Just board) <- parsed]
      pure [NamedBoard name board | (name, board) <- good]

-- | Load saves from a local directory
loadSaves :: IO [NamedSave]
loadSaves = do
  let dir = "saves"
  exists <- doesDirectoryExist dir -- Prevent crashes when no directory is found
  if not exists
    then pure []
    else do
      files <- listDirectory dir
      let fullPaths = map (dir </>) files
      realFiles <- filterM doesFileExist fullPaths

      decoded <- forM realFiles $ \fp -> do
        r <- decodeFileStrict fp
        pure (takeBaseName fp, r)

      let good = [(name, s) | (name, Just s) <- decoded]
      pure [NamedSave name s | (name, s) <- good]

