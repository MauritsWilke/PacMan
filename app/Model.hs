-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
module Model where
import GHC.Generics (Generic)
import Utils.Count (Timer, liveCounter, LiveCounter, ScoreCounter, RoundCounter, roundCounter, timeCounter, scoreCounter)
import Utils.Board
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key)

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
  -- GAME CONTROLS
  , keys         :: S.Set Key
  , size         :: (Int,Int)
  , shouldQuit   :: Bool
  , debugView    :: Int
  } deriving (Show, Generic)


initialState :: GameState
initialState = GameState
  { scene = Homescreen
  , level = initialLevelTEMP
  , player = initialPlayerTEMP
  , timer = timeCounter 0
  , lives = liveCounter 3
  , score = scoreCounter 0
  , Model.round = roundCounter 0
  , pelletsEaten = 0
  , ghostsEaten = 0
  , keys = S.empty
  , size = (400,400)
  , shouldQuit = False
  , debugView = 0
  }

initialLevelTEMP :: Level
initialLevelTEMP = Level
  { spawnPosition = (13, 13)
  , gameBoard = realBoard
  , ghosts = []
  }

initialPlayerTEMP :: Player
initialPlayerTEMP = Player
  { tilePosition = spawnPosition initialLevelTEMP
  , positionOffset = (0, 0)
  , direction = East
  , mode = Normal
  }

data Scene = Homescreen | LoadGame | ConfigureGame | SinglePlayer | MultiPlayer
  deriving (Show, Eq)

data Level = NoLevel | Level
  { spawnPosition :: (Int, Int) -- Spawn tile
  , gameBoard     :: Board
  , ghosts        :: [Ghost]   -- Custom amount of ghosts
  } deriving (Show, Generic)

-- Used to maintain movement without inputs
-- Cardinal directions used to prevent conflict with Gloss Up Down
data Direction = North | South | West | East
  deriving (Show, Eq)

data PlayerMode = Normal | Powered | Dead | Respawning | LevelComplete
  deriving (Show, Eq)

data Player = NoPlayer | Player
  { tilePosition  :: (Int, Int)
  , positionOffset :: (Double, Double)
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

tileWidth :: GameState -> Float
tileWidth gstate | arS < arB = sW / bW
                 | otherwise = sH / (3+bH)
 where arS = sW / sH
       arB = bW / bH
       bW = fromIntegral $ width $ gameBoard $ level gstate
       bH = fromIntegral $ height $ gameBoard $ level gstate
       sW = fromIntegral $ fst (size gstate)
       sH = fromIntegral $ snd (size gstate)
-- aspectRatio -> if aspectScreen < aspectBoard -> use full width 


-- halfTile :: GameState -> Float
-- halfTile gs = 0.5 * tileWidth gs -- need fixing