module Utils.Text where
import Graphics.Gloss

-- ! HEAVILY WORK IN PROGRESS

data TextSize = SmallText | MediumText | LargeText
  deriving (Show, Eq)

data TextWeight
  = RegularWeight
  | MediumWeight
  | BoldWeight
  deriving (Show, Eq)

data Align
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show, Eq)

-- | Tilewidth as first arg
textBlock :: Float -> TextSize -> TextWeight -> Align -> [String] -> Picture
textBlock tw tsize tweight align lines' =
  Pictures $
    zipWith (makeLine tsize tweight align) [0..] lines'
  where
    lineHeight = sizeScale tw tsize * 4 * tw
    makeLine :: TextSize -> TextWeight -> Align -> Int -> String -> Picture
    makeLine sz wt al index str =
      let s  = sizeScale tw sz
          y  = - (fromIntegral index * lineHeight)
          w  = estimateTextWidth tw str * s
          x  = case al of
                 AlignLeft   -> 0
                 AlignCenter -> - (w / 2)
                 AlignRight  -> -w
      in Translate x y (weightPicture wt s str)

weightPicture :: TextWeight -> Float -> String -> Picture
weightPicture RegularWeight s str = Scale s s (Text str)
weightPicture MediumWeight  s str = 
  Scale s s 
  $ Pictures
      [ Translate 0.5  0    (Text str)
      , Translate (-0.5) 0  (Text str)
      , Text str
      ]
weightPicture BoldWeight s str =
  Scale s s 
  $ Pictures
      [ Translate   1    0 (Text str)
      , Translate (-1)   0 (Text str)
      , Translate   0    1 (Text str)
      , Translate   0  (-1) (Text str)
      , Text str
      ]

sizeScale :: Float -> TextSize -> Float
sizeScale tw SmallText  = tw / 256
sizeScale tw MediumText = tw / 128
sizeScale tw LargeText  = tw / 32

estimateTextWidth :: Float -> String -> Float
estimateTextWidth tw str = sum (map (charWidth tw) str)

charWidth :: Float -> Char -> Float
charWidth tw c
  | c == ' '              = 3 * tw
  | c `elem` narrowChars  = 0.5 * tw
  | c `elem` wideChars    = 3 * tw
  | otherwise             = 2.5 * tw
  where
    narrowChars = "il.,!:;|"
    wideChars   = "MW@#"