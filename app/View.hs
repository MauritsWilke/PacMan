{-# LANGUAGE RecordWildCards #-} -- godsent
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Convert the game to a picture that can be displayed
module View where

import Model
import Graphics.Gloss
import View.Level

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate
  | debugView gstate == 0 = viewDefault gstate
  | otherwise             = Pictures [viewDefault gstate, viewDebug gstate]

viewDefault :: GameState -> Picture
viewDefault gstate 
  = Pictures
  [
    drawLevel (level gstate),
    drawPlayer (player gstate)
  ]

viewDebug :: GameState -> Picture
viewDebug gstate 
  = Pictures
  [
    drawLevelDebug (debugView gstate) (level gstate)
  ]

drawPlayer :: Player -> Picture
drawPlayer NoPlayer = blank
drawPlayer Player{..} = Color yellow $ circleSolid 10 -- TODO does not take the player position into account now