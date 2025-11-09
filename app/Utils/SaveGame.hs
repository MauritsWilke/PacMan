module Utils.SaveGame where

import Model
import Data.Aeson (encodeFile)

-- return gamestate after saving it
saveGameState :: FilePath -> GameState -> IO GameState
saveGameState path gs = do 
  encodeFile path (toSaveGameState gs)
  return gs { shouldSave = False }