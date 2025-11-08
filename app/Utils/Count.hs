{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Utils.Count where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Basic wrapper
newtype NonNeg = NonNeg Int
  deriving (Eq, Generic)
  deriving (ToJSON, FromJSON) via Int

class Count a where
  getCount :: a -> Int
  (.-) :: a -> Int -> a
  (.+) :: a -> Int -> a
infixl 6 .-, .+

instance Count NonNeg where
  getCount :: NonNeg -> Int
  getCount (NonNeg a) = a
  (.-) :: NonNeg -> Int -> NonNeg
  NonNeg a .- b = NonNeg (max 0 (a - b))
  (.+) :: NonNeg -> Int -> NonNeg
  NonNeg a .+ b = NonNeg (max 0 (a + b))

instance Show NonNeg where
  show :: NonNeg -> String
  show (NonNeg a) = show a 

-- Counters
newtype LevelCounter     = LevelCounter   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype LiveCounter      = LiveCounter    Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype RoundCounter     = RoundCounter   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype Timer            = Timer          Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype FrightTimer      = FrightTimer    Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype ReleaseTimer     = ReleaseTimer   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype ScoreCounter     = ScoreCounter   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype FruitTimer       = FruitTimer     Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype PoweredTimer     = PoweredTimer   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype ScatterTimer     = ScatterTimer   Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)
newtype AnimationTimer   = AnimationTimer Int   deriving (Eq, Show, Count) via NonNeg deriving (Generic, ToJSON, FromJSON)

animationTimer :: Int -> AnimationTimer
animationTimer = AnimationTimer . max 0

levelCounter :: Int -> LevelCounter
levelCounter = LevelCounter . max 0

liveCounter :: Int -> LiveCounter
liveCounter = LiveCounter . max 0

roundCounter :: Int -> RoundCounter
roundCounter = RoundCounter . max 0

timeCounter :: Int -> Timer
timeCounter = Timer . max 0

frightTimeCounter :: Int -> FrightTimer
frightTimeCounter = FrightTimer . max 0

releaseTimeCounter :: Int -> ReleaseTimer
releaseTimeCounter = ReleaseTimer . max 0

fruitTimeCounter :: Int -> FruitTimer
fruitTimeCounter = FruitTimer . max 0

poweredTimeCounter :: Int -> PoweredTimer
poweredTimeCounter = PoweredTimer . max 0

scoreCounter :: Int -> ScoreCounter
scoreCounter = ScoreCounter . max 0

scatterTimeCounter :: Int -> ScatterTimer
scatterTimeCounter = ScatterTimer . max 0
