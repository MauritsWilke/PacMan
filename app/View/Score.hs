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

drawScore :: Board -> Int -> Picture
drawScore Board{..} s =
  Translate ((fromIntegral width - 5) * halfTile) (fromIntegral height * halfTile + 0.5 * halfTile)
  $ Scale 0.1 0.1
  $ Color white
  $ Text (commas (show s))