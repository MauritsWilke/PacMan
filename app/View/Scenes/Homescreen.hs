module View.Scenes.Homescreen where
import Graphics.Gloss
import Model
import System.Random (mkStdGen, randomRs, splitGen)

renderHomescreen :: (Int, Int) -> TileWidth -> Picture
renderHomescreen (sw, sh) tw = Pictures $
  randomPacmans (sw, sh) tw ++
  [ Translate ((-7) * tw) 0
      $ Color white
      $ Scale (tw / 32) (tw / 32)
      $ Text "PacMan"
  , Translate ((-5.25) * tw) (- (tw * 2))
      $ Color white
      $ Scale (tw / 128) (tw / 128)
      $ Text "press space to start"
  , Translate ((-6.5625) * tw) (- (tw * 4))
      $ Color white
      $ Scale (tw / 128) (tw / 128)
      $ Text "press s to configure game"
  ]


randomPacmans :: (Int, Int) -> Float -> [Picture]
randomPacmans (sw, sh) tw =
  let g0       = mkStdGen 2609
      pacmans  = 120
      (gx, gy) = splitGen g0
      halfW    = fromIntegral sw / 2
      halfH    = fromIntegral sh / 2
      xs       = take pacmans (randomRs (-halfW, halfW) gx)
      ys       = take pacmans (randomRs (-halfH, halfH) gy)
  in zipWith (\x y -> Translate x y (drawPlayer tw)) xs ys

drawPlayer :: Float -> Picture 
drawPlayer tw = Color (withAlpha 0.6 yellow) $ circleSolid (0.5 * tw)