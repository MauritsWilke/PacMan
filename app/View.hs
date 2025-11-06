-- Convert the game to a picture that can be displayed
module View where

import Graphics.Gloss

import Model
import Utils.Count

import View.Level
import View.Player
import View.Ghost
import View.Score (drawScore)
import View.Scenes.Homescreen (renderHomescreen)
import View.Lives (drawLives)
import View.Paused

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gs = case scene gs of
    Homescreen    -> renderHomescreen (tileWidth gs)
    LoadGame      -> blank
    ConfigureGame -> blank
    SinglePlayer  -> if debugView gs == 0
                     then viewDefault gs
                     else Pictures [viewDefault gs, viewDebug gs]
    MultiPlayer   -> blank

viewDefault :: GameState -> Picture
viewDefault gs =
    let i = if poweredTimer gs == poweredTimeCounter 0 then 2 else 1
    in Pictures $ insertAt i (viewGhosts gs)
       [ drawLevel' gs
       , drawPlayer' gs
       , viewGUI gs
       ]

drawLevel' :: GameState -> Picture
drawLevel' gs = drawLevel (tileWidth gs) (level gs)

drawPlayer' :: GameState -> Picture
drawPlayer' gs = drawPlayer (tileWidth gs) (gameBoard (level gs)) (player gs)

viewGhosts :: GameState -> Picture
viewGhosts gs = Pictures $ map (drawGhost (tileWidth gs) (gameBoard (level gs))) (ghosts (level gs))

viewGhostsDebug :: GameState -> Picture
viewGhostsDebug gs = Pictures $ map (drawGhostDebug gs (tileWidth gs) (debugView gs) (gameBoard (level gs))) (ghosts (level gs))

viewGUI :: GameState -> Picture
viewGUI gs =
    Pictures
      [ drawScore  (tileWidth gs) (gameBoard (level gs)) (score gs)
      , drawLives  (tileWidth gs) (gameBoard (level gs)) 3
      , drawPaused (tileWidth gs) (gameBoard (level gs)) (paused gs)
      ]

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
    let (before, after) = splitAt i xs
    in before ++ [x] ++ after

viewDebug :: GameState -> Picture
viewDebug gs =
    let i = if poweredTimer gs == poweredTimeCounter 0 then 2 else 1
    in Pictures $ insertAt i (viewGhostsDebug gs)
      [ drawLevelDebug  (tileWidth gs) (debugView gs) (level gs)
      , drawPlayerDebug (tileWidth gs) (debugView gs) (gameBoard (level gs)) (player gs)
      ]
