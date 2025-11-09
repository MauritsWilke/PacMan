module Actions.Reset where

import Model
import Utils.Count
import qualified Data.Set as S

reset :: GameState -> GameState
reset gs = gs
  { level        = initialLevel
  , player       = initialPlayerTEMP
  -- COUNTERS (ALL MUST BE RESET)
  , timer        = timeCounter 0
  , lives        = liveCounter 3
  , score        = scoreCounter 0
  , Model.round  = roundCounter 0
  , animation    = animationTimer 0
  -- ROUND SPECIFIC, ALL MUST BE RESET
  , livesAwarded = 0
  , pelletsEaten = 0
  , ghostsEaten  = 0
  -- META RESET
  , keys         = S.empty
  , paused       = False
  , menuHelper   = 0
  }