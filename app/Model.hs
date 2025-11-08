-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where
import GHC.Generics (Generic)
import Utils.Count
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key)
import qualified Data.IntMap as I
import Data.Aeson
import Prelude hiding (round)

data GameState = GameState
  { scene        :: Scene
  , level        :: Level
  , player       :: Player
  , boards       :: [NamedBoard]
  , saves        :: [NamedSave]
  -- COUNTERS
  , timer        :: Timer        -- >=0 
  , lives        :: LiveCounter  -- >=0
  , score        :: ScoreCounter -- >=0
  , round        :: RoundCounter -- > 0
  -- ROUND SPECIFIC
  , pelletsEaten :: Int
  , ghostsEaten  :: Int -- Resets when eating power pellet
  -- , poweredTimer :: PoweredTimer
  -- GAME CONTROLS
  , keys         :: S.Set Graphics.Gloss.Interface.IO.Game.Key
  , screenSize   :: (Int,Int)
  , shouldQuit   :: Bool
  , shouldSave   :: Bool
  , paused       :: Bool
  , debugView    :: Int
  , menuHelper   :: Int -- Used for keeping track of selected item
  } deriving (Show, Generic)

data SaveGameState = SaveGameState
  { levelSave        :: Level
  , playerSave       :: Player
  , boardsSave       :: [NamedBoard]
  -- COUNTERS
  , timerSave        :: Timer        -- >=0 
  , livesSave        :: LiveCounter  -- >=0
  , scoreSave        :: ScoreCounter -- >=0
  , roundSave        :: RoundCounter -- > 0
  -- ROUND SPECIFIC
  , pelletsEatenSave :: Int
  , ghostsEatenSave  :: Int -- Resets when eating power pellet
  -- , poweredTimerSave :: PoweredTimer
  } deriving (Show, Generic, ToJSON, FromJSON)

toSaveGameState :: GameState -> SaveGameState
toSaveGameState gs = SaveGameState
  { levelSave = level gs
  , playerSave = player gs
  , boardsSave = boards gs
  -- COUNTERS
  , timerSave = timer gs
  , livesSave = lives gs
  , scoreSave = score gs
  , roundSave = round gs
  -- ROUND SPECIFIC
  , pelletsEatenSave = pelletsEaten gs
  , ghostsEatenSave = ghostsEaten gs
  -- , poweredTimerSave = poweredTimer gs
  }

initialState :: [NamedBoard] -> [NamedSave] -> GameState
initialState bs ss = GameState
  { scene        = Homescreen
  , level        = initialLevel bs
  , player       = initialPlayerTEMP
  , boards       = bs
  , saves        = ss
  -- COUNTERS
  , timer        = timeCounter 0
  , lives        = liveCounter 3
  , score        = scoreCounter 0
  , Model.round  = roundCounter 0
  -- ROUND SPECIFIC
  , pelletsEaten = 0
  , ghostsEaten  = 0
  -- , poweredTimer = poweredTimeCounter 0
  -- GAME CONTROLS
  , keys         = S.empty
  , screenSize   = (400,400)
  , shouldQuit   = False
  , shouldSave   = False
  , paused       = False
  , debugView    = 0
  , menuHelper   = 0
  }

initialLevel :: [NamedBoard] -> Level
initialLevel b = Level
  { spawnPosition = (13.5, 14)
  , gameBoard = boardData firstBoard
  , nameBoard = boardName firstBoard
  , ghosts = standardGhosts $ boardData firstBoard
  } where firstBoard = head b

initialLevelTEMP :: Level
initialLevelTEMP = Level
  { spawnPosition = (13.5, 14)
  , gameBoard = realBoard
  , nameBoard = "realBoard"
  , ghosts = standardGhosts realBoard
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
  , queuedDir = East
  , mode = Normal
  }

standardGhosts :: Board -> [Ghost]
standardGhosts b =
  [ createGhost 0 (getGhostSpawn b 0) Blinky
  , createGhost 1 (getGhostSpawn b 1) Inky
  , createGhost 2 (getGhostSpawn b 2) Pinky
  , createGhost 3 (getGhostSpawn b 3) Clyde
  ]

getPlayerSpawn :: Board -> (Float, Float)
getPlayerSpawn Board{..} 
 | null ints = (1.5,1.5)
 | otherwise = parseToMiddle $ indexToCoord (fst (head ints)) width
  where ints    = Prelude.filter ((== PlayerSpawn) . snd) (I.toList board)

-- used for eaten ghosts to return to spawn
getGhostSpawn :: Board -> Int -> (Float, Float)
getGhostSpawn Board{..} ghostIndex = parseToMiddle $ indexToCoord (fst (ints !! index)) width
  where ints    = Prelude.filter ((== GhostSpawn) . snd) (I.toList board)
        index   = ghostIndex `mod` length ints

getGhostExit :: Board -> (Float,Float)
getGhostExit Board{..} = parseToMiddle $ indexToCoord (fst (head ints)) width
  where ints    = Prelude.filter ((== GhostExit) . snd) (I.toList board)

parseToMiddle :: (Int,Int) -> (Float,Float)
parseToMiddle (x,y) = (fromIntegral x + 0.5, fromIntegral y + 0.5)

indexToCoord :: Int -> Int -> (Int, Int)
indexToCoord i w = (i `div` w, i `mod` w)

data Scene = Homescreen | LoadGame | ConfigureGame | SinglePlayer | MultiPlayer | Paused
  deriving (Show, Eq)

data Level = NoLevel | Level
  { spawnPosition :: (Float, Float) -- Spawn tile
  , gameBoard     :: Board
  , nameBoard     :: String    -- TODO combine these into NamedBoard, veel werk!
  , ghosts        :: [Ghost]   -- Custom amount of ghosts
  } deriving (Show, Generic, ToJSON, FromJSON)

data Tile = Wall | Empty | Pellet | PowerPellet | Fruit | GhostSpawn | GhostExit | PlayerSpawn
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Board = Board
  { board :: I.IntMap Tile
  -- , wallMap :: I.IntMap Wall
  , width :: Int
  , height:: Int
  } deriving (Show, Generic)

instance ToJSON Board
instance FromJSON Board

-- Used to maintain movement without inputs
-- Cardinal directions used to prevent conflict with Gloss Up Down
data Direction = North | South | West | East
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PlayerMode = Normal | Dead | Respawning | LevelComplete
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Player = NoPlayer | Player
  { position  :: (Float, Float) -- offset for player
  , direction :: Direction
  , queuedDir :: Direction
  , mode      :: PlayerMode
  } deriving (Show, Generic, ToJSON, FromJSON)

data GhostType = Inky | Blinky | Pinky | Clyde
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GhostMode = Chase | Scatter | Fright | Spawn
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Ghost = Ghost
  { ghostType      :: GhostType
  , ghostMode      :: GhostMode
  , ghostPosition  :: (Float,Float)
  , ghostDirection :: Direction
  , releaseIndex   :: Int          -- defines the order of release
  , destination    :: Maybe (Float, Float) -- locks all interactions until at destination
  , frightTimer    :: FrightTimer  -- >=0, counts down
  , releaseTimer   :: ReleaseTimer -- >=0, counts down
  , scatterTimer   :: ScatterTimer -- >=0, counts down
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

createGhost :: Int -> (Float,Float) -> GhostType -> Ghost
createGhost orderIndex spawn typ = Ghost
  { ghostType      = typ
  , ghostMode      = Chase
  , ghostPosition  = spawn
  , ghostDirection = North
  , releaseIndex   = orderIndex
  , destination     = Nothing
  , scatterTimer   = scatterTimeCounter 0
  , frightTimer    = frightTimeCounter 0
  , releaseTimer   = releaseTimeCounter (orderIndex * 5 * 60)
  }

data NamedBoard = NamedBoard
  { boardName :: String
  , boardData :: Board
  } deriving (Show, Generic)

instance ToJSON NamedBoard
instance FromJSON NamedBoard

data NamedSave = NamedSave
  { saveName :: String
  , saveData :: SaveGameState
  } deriving (Show)

-- NAME UTILS
type TileWidth       = Float
type TileCoordinates = (Int, Int)
type Speed           = Float
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

-- get player speed based on round
playerSpeed :: GameState -> Float
playerSpeed gstate | roundIndex > 20 = 0.1425
                   | roundIndex > 4  = 0.155
                   | otherwise       = 0.13
  where roundIndex = getCount $ Model.round gstate

-- get ghost speed based on round
ghostSpeed :: GameState -> Float
ghostSpeed gstate  | roundIndex > 4  = 0.14875
                   | otherwise       = 0.12375
  where roundIndex = getCount $ Model.round gstate

--get all ghosts that currently are in frightened mode
frightenedGhosts :: [Ghost] -> [Ghost]
frightenedGhosts []     = []
frightenedGhosts (x:xs) = if frightened x then x : remainder else remainder
  where frightened Ghost{..} = getCount frightTimer > 0
        remainder = frightenedGhosts xs