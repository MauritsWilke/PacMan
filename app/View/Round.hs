{-# LANGUAGE RecordWildCards #-}
module View.Round where
import Graphics.Gloss
import Model
import Utils.Count (RoundCounter)

drawRoundIndicator :: TileWidth -> Board -> RoundCounter -> Picture
drawRoundIndicator tw Board{..} s =
  Translate (- (fromIntegral width * (0.5 * tw))) (fromIntegral height * (0.5 * tw) + (0.5 * tw))
  $ Scale (tw / 250) (tw / 250)
  $ Color white
  $ Text t
  where t = "Round " ++ show s