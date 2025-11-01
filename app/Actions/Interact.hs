module Actions.Interact where

import Model
import Utils.Board as GB
import Data.Bifunctor (bimap)

updateLevel :: GameState -> Level
updateLevel gs = lvl { gameBoard = interactPellets brd plr }
  where 
    lvl = level gs
    brd = gameBoard lvl
    plr = player gs


interactPellets :: Board -> Player -> Board
interactPellets b p = 
  case GB.get playerPos b of
    Just Pellet -> GB.set playerPos Empty b 
    _           -> b
  where playerPos = bimap floor floor (position p) 
  