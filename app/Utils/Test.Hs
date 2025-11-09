module Utils.Test where

standardGhosts :: Board -> [Ghost]
standardGhosts b =
  [ createGhost (getGhostSpawn b) Blinky
  , createGhost (getGhostSpawn b) Inky
  , createGhost (getGhostSpawn b) Pinky
  , createGhost (getGhostSpawn b) Clyde
  ]