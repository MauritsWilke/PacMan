-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
module Model where
import GHC.Generics (Generic)
import Utils.Count
import Utils.Board
import Control.Monad.ST (ST)

-- ! Oprecht geen idee hoezo dit niet geimport hoeft te worden met data Board ipv type Board
-- import GHC.Arr (STArray) 

data GameState s = GameState 
  { scene        :: Scene
  , level        :: Level s
  , player       :: Player
  -- COUNTERS
  , timer        :: Int -- >=0 
  , lives        :: Int -- >=0
  , score        :: Int -- >=0
  , round        :: Int -- > 0
  -- ROUND SPECIFIC
  , pelletsEaten :: Int
  , ghostsEaten  :: Int -- Resets when eating power pellet
  } deriving (Show, Generic)

initialState :: ST s (GameState s)
initialState = do
  pure $ GameState Homescreen NoLevel NoPlayer 0 0 0 0 0 0

data Scene = Homescreen | LoadGame | ConfigureGame | SinglePlayer | MultiPlayer
  deriving (Show, Eq)

data Level s = NoLevel | Level 
  { spawnPosition :: (Int, Int) -- Spawn tile
  , board         :: Board
  , ghosts        :: [Ghost]   -- Custom amount of ghosts
  } deriving (Show, Generic)

-- Use (row, col) for indexing


-- Used to maintain movement without inputs
data Direction = Up | Down | Left | Right
  deriving (Show, Eq)
  
data PlayerMode = Normal | Powered | Dead | Respawning | LevelComplete
  deriving (Show, Eq)

data Player = NoPlayer | Player 
  { position  :: (Double, Double)
  , direction :: Direction
  , mode      :: PlayerMode
  } deriving (Show)

data GhostType = Inky | Blinky | Pinky | Clyde
  deriving (Show, Eq)
  
data GhostMode = Chase | Scatter | Fright | Spawn
  deriving (Show, Eq)

data Ghost = Ghost
  { ghostType    :: GhostType
  , ghostMode    :: GhostMode
  , freightTimer :: Int -- >=0, counts down
  , releaseTimer :: Int -- >=0, counts down
  } deriving (Show)