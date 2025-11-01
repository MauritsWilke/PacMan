{-# LANGUAGE RecordWildCards #-}
module View.Score where
import Graphics.Gloss
import Model
import Utils.Count (ScoreCounter)

commas :: String -> String
commas = reverse . commas' . reverse
  where
    commas' :: String -> String
    commas' [] = ""
    commas' [x, y, z] = [x, y, z]
    commas' (x:y:z:xs) = [x, y, z, ','] ++ commas' xs
    commas' xs = xs

drawScore :: TileWidth -> Board -> ScoreCounter -> Picture
drawScore tw Board{..} s =
  Translate ((fromIntegral width - 5) * (0.5 * tw)) (fromIntegral height * (0.5 * tw) + (0.5 * tw))
  $ Scale (tw / 250) (tw / 250)
  $ Color white
  $ Text (commas (show s))