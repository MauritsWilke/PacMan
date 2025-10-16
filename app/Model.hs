-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
module Model where
import GHC.Generics (Generic)
import Utils.Count (Timer, liveCounter, LiveCounter, ScoreCounter, RoundCounter, roundCounter, timeCounter, scoreCounter)
import Utils.Board

-- ! Oprecht geen idee hoezo dit niet geimport hoeft te worden met data Board ipv type Board
-- import GHC.Arr (STArray) 

data GameState = GameState 
  { scene        :: Scene
  , level        :: Level
  , player       :: Player
  -- COUNTERS
  , timer        :: Timer -- >=0 
  , lives        :: LiveCounter -- >=0
  , score        :: ScoreCounter -- >=0
  , round        :: RoundCounter -- > 0
  -- ROUND SPECIFIC
  , pelletsEaten :: Int
  , ghostsEaten  :: Int -- Resets when eating power pellet
  } deriving (Show, Generic)


initialState :: GameState
initialState = GameState {scene = Homescreen, level = NoLevel, player = NoPlayer, timer = timeCounter 0, lives = liveCounter 3, score = scoreCounter 0, Model.round = roundCounter 0, pelletsEaten = 0, ghostsEaten = 0}

data Scene = Homescreen | LoadGame | ConfigureGame | SinglePlayer | MultiPlayer
  deriving (Show, Eq)

data Level = NoLevel | Level 
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