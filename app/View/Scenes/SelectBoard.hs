module View.Scenes.SelectBoard where
import Graphics.Gloss
import Model
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace (traceShow)

-----------------------------
-- DEFAULT SCENE FUNCTIONS --
-----------------------------

enterScene :: GameState -> GameState
enterScene gs0 =
  let
    count = length (boards gs0)
    lo = 0
    hi = count - 1

    clamped = localiseMenuHelper lo hi (menuHelper gs0)
  in gs0 { menuHelper = clamped }

exitScene :: GameState -> GameState
exitScene gs0 =
  let idx = menuHelper gs0
      brd = boards gs0 !! idx
      lvl = level gs0
      newLevel = lvl
        { nameBoard = boardName brd
        , gameBoard = boardData brd
        }
  in traceShow ("EXIT:", nameBoard newLevel) $ gs0 { level = newLevel }

controlScene :: Key -> GameState -> GameState
controlScene (Char 's') gs = gs { menuHelper = min (menuHelper gs + 1) ((length . boards) gs - 1) }
controlScene (Char 'w') gs = gs { menuHelper = max (menuHelper gs - 1) 0 }
controlScene _          gs = gs

-----------------------------
-- RENDERING FUNCTIONS     --
-----------------------------

oneLine :: Float -> Float
oneLine tw = - (2 * tw)

-- min val, max val, curr val
localiseMenuHelper :: Int -> Int -> Int -> Int
localiseMenuHelper lo hi x = max lo (min hi x)

renderBoardSelection :: GameState -> Picture
renderBoardSelection gs0 =
  let
    count = length (boards gs0)
    lo = 0
    hi = count - 1

    clamped = localiseMenuHelper lo hi (menuHelper gs0)
    gs = gs0 { menuHelper = clamped }
  in
    Translate (-(5 * tw)) 0 $
    Pictures $
      zipWith (\i v -> Translate 0 (i * oneLine tw) v) [0..]
        ( [ header ] ++ renderBoards gs ++ [ footer ] )
  where
    tw = tileWidth gs0
    header = Color white $ Scale (tw/128) (tw/128) $ Text "Select a board:"
    footer = Color white $ Scale (tw/128) (tw/128) $ Text "Press enter to select"


renderBoards :: GameState -> [Picture]
renderBoards gs = map draw brds
  where
    tw            = tileWidth gs
    currentBoard  = nameBoard (level gs)
    selectedIndex = menuHelper gs
    brds          = zip [0..] (boards gs)

    draw (i, brd) =
      let name = boardName brd
          prefix = if i == selectedIndex then "> " else ""
          suffix = if name == currentBoard then " (current)" else ""
      in Color white
         $ Scale (tw / 128) (tw / 128)
         $ Text (prefix ++ name ++ suffix)
