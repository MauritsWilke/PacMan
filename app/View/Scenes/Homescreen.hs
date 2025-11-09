module View.Scenes.Homescreen where
import Graphics.Gloss
import Model
import System.Random (mkStdGen, randomRs, splitGen)
import View.Player

-- | Renders the home screen
renderHomescreen :: Bool -> Bool -> (Int, Int) -> TileWidth -> Picture
renderHomescreen areSaves areBoards (sw, sh) tw = Pictures $
  randomPacmans (sw, sh) tw ++
  [ Translate 0 (- tw)
      $ Color (makeColor 0 0 0 0.7)
      $ rectangleSolid (20 * tw) (16 * tw)      -- improve contrast with background pattern
  , Translate ((-7) * tw) 0
      $ Color white
      $ Scale (tw / 32) (tw / 32)               -- large header size
      $ Text "PacMan"
  , Translate ((-5.25) * tw) (- (tw * 2))       -- 0.2625 * 20
      $ Color white
      $ Scale (tw / 128) (tw / 128)             -- medium text size
      $ Text "press space to start"
  ]
  ++ [ Translate ((-6.5625) * tw) (- (tw * 4))  -- -0.2625 * 27
         $ Color white
         $ Scale (tw / 128) (tw / 128)          -- medium text size
         $ Text "press 's' to configure game"
     | areBoards ]                              -- only show when there are any boards
  ++ [ Translate ((-6.5625) * tw) (- (tw * 6))
         $ Color white
         $ Scale (tw / 128) (tw / 128)          -- medium text size
         $ Text "press 'l' to load a save"
     | areSaves ]                               -- only show when there are any saves

-- | Load a nice wallpaper for the home screen
-- | Using set seed over true random because it is pure
randomPacmans :: (Int, Int) -> Float -> [Picture]
randomPacmans (sw, sh) tw =
  let g0       = mkStdGen 2609 -- I like this number
      pacmans  = 120           -- Same goes for this one
      (gx, gy) = splitGen g0
      halfW    = fromIntegral sw / 2
      halfH    = fromIntegral sh / 2
      xs       = take pacmans (randomRs (-halfW, halfW) gx)
      ys       = take pacmans (randomRs (-halfH, halfH) gy)
  in zipWith (\x y -> Translate x y (openMouth tw)) xs ys