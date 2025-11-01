-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
module Model where
import GHC.Generics (Generic)
import Utils.Count (Timer, liveCounter, LiveCounter, ScoreCounter, RoundCounter, roundCounter, timeCounter, scoreCounter, freightTimer)
import qualified Utils.Count as C
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key)
import qualified Data.IntMap as I

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
  , screenSize   :: (Int,Int)
  , shouldQuit   :: Bool
  , paused       :: Bool
  , debugView    :: Int
  } deriving (Show, Generic)


initialState :: GameState
initialState = GameState
  { scene = SinglePlayer
  , level = initialLevelTEMP
  , player = initialPlayerTEMP
  , timer = timeCounter 0
  , lives = liveCounter 3
  , score = scoreCounter 0
  , Model.round = roundCounter 0
  , pelletsEaten = 0
  , ghostsEaten = 0
  , keys = S.empty
  , screenSize = (400,400)
  , shouldQuit = False
  , paused = False
  , debugView = 0
  }

initialLevelTEMP :: Level
initialLevelTEMP = Level
  { spawnPosition = (13.5, 14)
  , gameBoard = realBoard
  , ghosts = standardGhosts
  }

realBoard :: Board
realBoard = Board {
  board = I.fromList boardList,
  width = w,
  height = h
}
  where
    boardList = zip coordList tiles
    coordList = [0..((w * h) - 1)]
    w = 28
    h = 31
    tiles = [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,PowerPellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,PowerPellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Pellet,Empty,Empty,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Empty,Empty,Pellet,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Empty,Wall,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,GhostSpawn,Wall,Empty,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Wall,Wall,Wall,GhostExit,GhostExit,Wall,Wall,Wall,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,PowerPellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,PowerPellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Wall,Wall,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Pellet,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]

initialPlayerTEMP :: Player
initialPlayerTEMP = Player
  { position = spawnPosition initialLevelTEMP
  , direction = East
  , mode = Normal
  }

data Scene = Homescreen | LoadGame | ConfigureGame | SinglePlayer | MultiPlayer
  deriving (Show, Eq)

data Level = NoLevel | Level
  { spawnPosition :: (Float, Float) -- Spawn tile
  , gameBoard     :: Board
  , ghosts        :: [Ghost]   -- Custom amount of ghosts
  } deriving (Show, Generic)

data Tile = Wall | Empty | Pellet | PowerPellet | Fruit | GhostSpawn | GhostExit
  deriving (Show, Eq)

data Board = Board
  { board :: I.IntMap Tile
  -- , wallMap :: I.IntMap Wall
  , width :: Int
  , height:: Int
  } deriving (Show)

-- Used to maintain movement without inputs
-- Cardinal directions used to prevent conflict with Gloss Up Down
data Direction = North | South | West | East
  deriving (Show, Eq)

data PlayerMode = Normal | Powered | Dead | Respawning | LevelComplete
  deriving (Show, Eq)

data Player = NoPlayer | Player
  { position :: (Float, Float) -- offset for player
  , direction :: Direction
  , mode      :: PlayerMode
  } deriving (Show)

data GhostType = Inky | Blinky | Pinky | Clyde
  deriving (Show, Eq)

data GhostMode = Chase | Scatter | Fright | Spawn
  deriving (Show, Eq)

data Ghost = Ghost
  { ghostType      :: GhostType
  , ghostMode      :: GhostMode
  , ghostPosition  :: (Float,Float)
  , ghostDirection :: Direction
  , destination    :: Maybe (Float,Float)  -- only changed when at destination or when switching mode 
  , freightTimer   :: C.FreightTimer -- >=0, counts down
  , releaseTimer   :: C.ReleaseTimer -- >=0, counts down
  } deriving (Show)

standardGhosts :: [Ghost]
standardGhosts = 
  [ createGhost (1.5,1.5) Blinky
  , createGhost (2.5,1.5) Inky
  , createGhost (1.5,2.5) Pinky
  , createGhost (1.5,3.5) Clyde
  ]

createGhost :: (Float,Float) -> GhostType -> Ghost
createGhost spawn typ = Ghost 
  { ghostType = typ
  , ghostMode = Scatter
  , ghostPosition = spawn
  , ghostDirection = North
  , destination = Nothing
  , Model.freightTimer = (C.freightTimer 0)
  , releaseTimer = (C.releaseTimer 0)
  }

-- NAME UTILS
type TileWidth       = Float
type TileCoordinates = (Int, Int)
type PlayerSpeed     = Float
type BoardWidth      = Int
type BoardHeight     = Int
type Score           = Int

tileWidth :: GameState -> Float
tileWidth gstate
  | aspectRatioScreen < aspectRatioBoard = screenWidth / boardWidth
  | otherwise                            = screenHeight / boardHeight
  where boardHeight       = fromIntegral (3 + height (gameBoard (level gstate))) -- additional 3 slots reserved for info display on top and bottom of the screen
        boardWidth        = fromIntegral $ width  $ gameBoard $ level gstate
        screenWidth       = fromIntegral $ fst (screenSize gstate)
        screenHeight      = fromIntegral $ snd (screenSize gstate)
        aspectRatioScreen = screenWidth / screenHeight
        aspectRatioBoard  = boardWidth  / boardHeight