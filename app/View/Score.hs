{-# LANGUAGE RecordWildCards #-}
module View.Score where
import Utils.Board
import Graphics.Gloss

commas :: String -> String
commas = reverse . commas' . reverse
  where
    commas' :: String -> String
    commas' [] = ""
    commas' (x:y:z:xs) = [x, y, z, ','] ++ commas' xs
    commas' xs = xs

drawScore :: Float -> Board -> Int -> Picture
drawScore tw Board{..} s =
  Translate ((fromIntegral width - 5) * (0.5 * tw)) (fromIntegral height * (0.5 * tw) + (0.5 * tw))
  $ Scale (tw / 250) (tw / 250)
  $ Color white
  $ Text (commas (show s))