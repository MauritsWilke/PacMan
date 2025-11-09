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
import View.Scenes.SelectBoard (renderBoardSelection)
import View.Scenes.LoadSave
import View.GameOver (drawGameOver)
import View.Round

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gs = case scene gs of
    Homescreen    -> renderHomescreen ((not . null . saves) gs) ((not . null . boards) gs) (screenSize gs) (tileWidth gs)
    LoadGame      -> renderSaveselection gs
    ConfigureGame -> renderBoardSelection gs
    SinglePlayer  -> if debugView gs == 0
                     then viewDefault gs
                     else Pictures [viewDefault gs, viewDebug gs]
    Paused        -> if debugView gs == 0
                     then viewDefault gs
                     else Pictures [viewDefault gs, viewDebug gs]
    MultiPlayer   -> blank
    GameOver      -> if debugView gs == 0
                     then viewDefault gs
                     else Pictures [viewDefault gs, viewDebug gs]

viewDefault :: GameState -> Picture
viewDefault gs =
    let i = if null (frightenedGhosts (ghosts (level gs))) then 2 else 1
    in Pictures $ insertAt i (viewGhosts gs)
       [ drawLevel' gs
       , drawPlayer' gs
       , viewGUI gs
       ]

drawLevel' :: GameState -> Picture
drawLevel' gs = drawLevel (tileWidth gs) (level gs)

drawPlayer' :: GameState -> Picture
drawPlayer' gs = drawPlayer (animation gs) (tileWidth gs) (gameBoard (level gs)) (player gs)

viewGhosts :: GameState -> Picture
viewGhosts gs = Pictures $ map (drawGhost (animation gs) (tileWidth gs) (gameBoard (level gs))) (ghosts (level gs))

viewGhostsDebug :: GameState -> Picture
viewGhostsDebug gs = Pictures $ map (drawGhostDebug gs (tileWidth gs) (debugView gs) (gameBoard (level gs))) (ghosts (level gs))

viewGUI :: GameState -> Picture
viewGUI gs =
    Pictures
      [ drawScore    (tileWidth gs) (gameBoard (level gs)) (score gs)
      , drawLives    (tileWidth gs) (gameBoard (level gs)) (getCount (lives gs))
      , drawPaused   (tileWidth gs) (gameBoard (level gs)) (scene gs == Paused)
      , drawGameOver (tileWidth gs) (gameBoard (level gs)) (scene gs == GameOver) (score gs)
      , drawRoundIndicator (tileWidth gs) (gameBoard (level gs)) (Model.round gs)
      ]

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
    let (before, after) = splitAt i xs
    in before ++ [x] ++ after

viewDebug :: GameState -> Picture
viewDebug gs =
    let i = if null (frightenedGhosts (ghosts (level gs))) then 2 else 1
    in Pictures $ insertAt i (viewGhostsDebug gs)
      [ drawLevelDebug  (tileWidth gs) (debugView gs) (level gs)
      , drawPlayerDebug (tileWidth gs) (debugView gs) (gameBoard (level gs)) (player gs)
      ]
