module Utils.PlayerUtil where
import Model
import Actions.Move
import Utils.Count

getPlayerPosition :: GameState -> (Float, Float)
getPlayerPosition = position . player

-- returns 
-- Nothing    -> if player doesn't hit ghost
-- Just True  -> if player hits ghost and player eats ghost
-- Just False -> if player hits ghost and ghost eats player
eats :: Player -> Ghost -> Maybe Bool
eats p g | hit && frighten = Just True
         | hit             = Just False
         | otherwise       = Nothing
 where frighten = getCount (frightTimer g) > 0 
       hit      = distance (position p) (ghostPosition g) < 0.5