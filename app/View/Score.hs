{-# LANGUAGE RecordWildCards #-}
module View.Score where
import Graphics.Gloss
import Model
import Utils.Count (ScoreCounter)

-- | Small util function to add numerical separators
-- | As the score is unlikely to grow above a couple million
-- | two reverses is not a performance hinder
commas :: String -> String
commas = reverse . commas' . reverse
  where
    commas' :: String -> String
    commas' [] = ""
    commas' [x, y, z] = [x, y, z]
    commas' (x:y:z:xs) = [x, y, z, ','] ++ commas' xs
    commas' xs = xs

-- | Draw the score to the top right of the screen
drawScore :: TileWidth -> Board -> ScoreCounter -> Picture
drawScore tw Board{..} s =
  Translate (- ((fromIntegral (length t) - 1) * 0.2 * tw)) 0 -- Ensure proper right-side alignment
  $ Translate ((fromIntegral width - 1) * (0.5 * tw)) (fromIntegral height * (0.5 * tw) + (0.5 * tw))
  $ Scale (tw / 250) (tw / 250)
  $ Color white
  $ Text t
  where t = commas (show s)