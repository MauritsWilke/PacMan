module View.Scenes.LoadSave where
import Graphics.Gloss
import Model
import Graphics.Gloss.Interface.IO.Game
import Prelude hiding (round)

-----------------------------
-- DEFAULT SCENE FUNCTIONS --
-----------------------------

enterScene :: GameState -> GameState
enterScene gs0 =
  let
    hi = length (saves gs0) - 1
    clamped = localiseMenuHelper 0 hi (menuHelper gs0)
  in gs0 { menuHelper = clamped }

exitScene :: GameState -> GameState
exitScene gs0 = 
  let idx = menuHelper gs0
      sav = saveData $ saves gs0 !! idx
  in gs0 
  { level        = levelSave sav
  , player       = playerSave sav
  , boards       = boardsSave sav
  , timer        = timerSave sav
  , lives        = livesSave sav
  , score        = scoreSave sav
  , round        = roundSave sav
  , ghostsEaten  = ghostsEatenSave sav
  }

controlScene :: Key -> GameState -> GameState
controlScene (Char 's') gs = gs { menuHelper = min (menuHelper gs + 1) ((length . saves) gs - 1) }
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

renderSaveselection :: GameState -> Picture
renderSaveselection gs =
    Translate (-(5 * tw)) 0
    $ Pictures
    $ zipWith (\i v -> Translate 0 (i * oneLine tw) v) [0..]
        ( [ header ] ++ rendersaves gs ++ [ footer ] )
  where
    tw = tileWidth gs
    header = Color white $ Scale (tw / 128) (tw / 128) $ Text "Select a save:"
    footer = Color white $ Scale (tw / 128) (tw / 128) $ Text "Press enter to select"


rendersaves :: GameState -> [Picture]
rendersaves gs = map draw svs
  where
    tw            = tileWidth gs
    selectedIndex = menuHelper gs
    svs          = zip [0..] (saves gs)

    draw (i, brd) =
      let name = saveName brd
          prefix = if i == selectedIndex then "> " else ""
      in Color white
         $ Scale (tw / 128) (tw / 128)
         $ Text (prefix ++ name)
