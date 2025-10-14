module Utils.Count (getCount, (Utils.Count.-), (Utils.Count.+), levelCounter, liveCounter, roundCounter, timer, freightTimer, releaseTimer) where

newtype LevelCounter = LevelCounter Int deriving (Show)
newtype LiveCounter = LiveCounter Int deriving (Show)
newtype RoundCounter = RoundCounter Int deriving (Show)
newtype Timer = Timer Int deriving (Show)
newtype FreightTimer = FreightTimer Int deriving (Show)
newtype ReleaseTimer = ReleaseTimer Int deriving (Show)
newtype FruitTimer = FruitTimer Int deriving (Show)

levelCounter :: Int -> LevelCounter
levelCounter a = LevelCounter (max 0 a)

liveCounter :: Int -> LiveCounter
liveCounter a = LiveCounter (max 0 a)

roundCounter :: Int -> RoundCounter
roundCounter a = RoundCounter (max 0 a)

timer :: Int -> Timer
timer a = Timer (max 0 a)

freightTimer :: Int -> FreightTimer
freightTimer a = FreightTimer (max 0 a)

releaseTimer :: Int -> ReleaseTimer
releaseTimer a = ReleaseTimer (max 0 a)

class Count a where
    getCount :: a -> Int
    (-) :: a -> Int -> a
    (+) :: a -> Int -> a

-- (1) level, (2) live, (3) timer, (4) round, (5) freightTimer, (6) releasetimer, (7) fruitTimer

instance Count LevelCounter where
    getCount (LevelCounter a) = a
    (-) (LevelCounter a) b = LevelCounter (max 0 (a Prelude.- b))
    (+) (LevelCounter a) b = LevelCounter (max 0 (a Prelude.+ b))

instance Count LiveCounter where
    getCount (LiveCounter a) = a
    (-) (LiveCounter a) b = LiveCounter (max 0 (a Prelude.- b))
    (+) (LiveCounter a) b = LiveCounter (max 0 (a Prelude.+ b))

instance Count RoundCounter where
    getCount (RoundCounter a) = a
    (-) (RoundCounter a) b = RoundCounter (max 0 (a Prelude.- b))
    (+) (RoundCounter a) b = RoundCounter (max 0 (a Prelude.+ b))

instance Count Timer where
    getCount (Timer a) = a
    (-) (Timer a) b = Timer (max 0 (a Prelude.- b))
    (+) (Timer a) b = Timer (max 0 (a Prelude.+ b))

instance Count FreightTimer where
    getCount (FreightTimer a) = a
    (-) (FreightTimer a) b = FreightTimer (max 0 (a Prelude.- b))
    (+) (FreightTimer a) b = FreightTimer (max 0 (a Prelude.+ b))

instance Count ReleaseTimer where
    getCount (ReleaseTimer a) = a
    (-) (ReleaseTimer a) b = ReleaseTimer (max 0 (a Prelude.- b))
    (+) (ReleaseTimer a) b = ReleaseTimer (max 0 (a Prelude.+ b))

instance Count FruitTimer where
    getCount (FruitTimer a) = a
    (-) (FruitTimer a) b = FruitTimer (max 0 (a Prelude.- b))
    (+) (FruitTimer a) b = FruitTimer (max 0 (a Prelude.+ b))