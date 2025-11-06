{-# LANGUAGE DerivingVia #-}

module Utils.Count where

newtype NonNeg = NonNeg Int

instance Show NonNeg where
  show (NonNeg a) = show a 

class Count a where
  getCount :: a -> Int
  (.-) :: a -> Int -> a
  (.+) :: a -> Int -> a
infixl 6 .-, .+

instance Count NonNeg where
  getCount (NonNeg a) = a
  NonNeg a .- b = NonNeg (max 0 (a - b))
  NonNeg a .+ b = NonNeg (max 0 (a + b))

newtype LevelCounter   = LevelCounter Int   deriving Eq deriving (Show, Count) via NonNeg
newtype LiveCounter    = LiveCounter  Int   deriving Eq deriving (Show, Count) via NonNeg
newtype RoundCounter   = RoundCounter Int   deriving Eq deriving (Show, Count) via NonNeg
newtype Timer          = Timer        Int   deriving Eq deriving (Show, Count) via NonNeg
newtype FreightTimer   = FreightTimer Int   deriving Eq deriving (Show, Count) via NonNeg
newtype ReleaseTimer   = ReleaseTimer Int   deriving Eq deriving (Show, Count) via NonNeg
newtype ScoreCounter   = ScoreCounter Int   deriving Eq deriving (Show, Count) via NonNeg
newtype FruitTimer     = FruitTimer   Int   deriving Eq deriving (Show, Count) via NonNeg
newtype PoweredTimer   = PoweredTimer Int   deriving Eq deriving (Show, Count) via NonNeg
newtype ScatterTimer   = ScatterTimer Int   deriving Eq deriving (Show, Count) via NonNeg


levelCounter :: Int -> LevelCounter
levelCounter   = LevelCounter . max 0
liveCounter :: Int -> LiveCounter
liveCounter    = LiveCounter . max 0
roundCounter :: Int -> RoundCounter
roundCounter   = RoundCounter . max 0
timeCounter :: Int -> Timer
timeCounter    = Timer . max 0
frightTimeCounter :: Int -> FreightTimer
frightTimeCounter   = FreightTimer . max 0
releaseTimeCounter :: Int -> ReleaseTimer
releaseTimeCounter   = ReleaseTimer . max 0
fruitTimeCounter :: Int -> FruitTimer
fruitTimeCounter     = FruitTimer . max 0
poweredTimeCounter :: Int -> PoweredTimer
poweredTimeCounter = PoweredTimer . max 0
scoreCounter :: Int -> ScoreCounter
scoreCounter   = ScoreCounter . max 0
scatterTimeCounter :: Int -> ScatterTimer
scatterTimeCounter   = ScatterTimer . max 0

