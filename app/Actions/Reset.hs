module Actions.Reset where

import Model
import Utils.Count
import qualified Data.Set as S

reset :: GameState -> GameState
reset gs = gs
  { level        = initialLevel (boards gs)
  , player       = initialPlayerTEMP
  , timer        = timeCounter 0
  , lives        = liveCounter 3
  , score        = scoreCounter 0
  , Model.round  = roundCounter 0
  , pelletsEaten = 0
  , ghostsEaten  = 0
  , poweredTimer = poweredTimeCounter 0
  , keys         = S.empty
  , paused       = False
  }