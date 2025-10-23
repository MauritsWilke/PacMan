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
drawScore tileW Board{..} s =
  Translate ((fromIntegral width - 5) * (0.5*tileW)) (fromIntegral height * (0.5*tileW) + (0.5*tileW))
  $ Scale 0.1 0.1
  $ Color white
  $ Text (commas (show s))