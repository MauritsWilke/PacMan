module Utils.PlayerUtil where
import Model

getPlayerPosition :: GameState -> (Float, Float)
getPlayerPosition = position . player