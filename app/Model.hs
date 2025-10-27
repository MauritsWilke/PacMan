-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
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
  , screenSize   :: (Int,Int)
  , shouldQuit   :: Bool
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
  , positionOffset :: (Double, Double) -- offset for player
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
tileWidth gstate
  | aspectRatioScreen < aspectRatioBoard = screenWidth / boardWidth
  | otherwise                            = screenHeight / boardHeight
  where aspectRatioScreen = screenWidth / screenHeight
        aspectRatioBoard  = boardWidth / boardHeight
        boardWidth        = fromIntegral $ width  $ gameBoard $ level gstate
        boardHeight       = fromIntegral (3 + height (gameBoard (level gstate))) -- additional 3 slots reserved for info display on top and bottom of the screen
        screenWidth       = fromIntegral $ fst (screenSize gstate)
        screenHeight      = fromIntegral $ snd (screenSize gstate)

move :: GameState -> Direction -> Player
move gs dir = (player gs) {tilePosition = position}
  where
    position =
     case moveIsPossible gs dir of
      Nothing -> getPlayerPosition gs
      Just a -> a   

moveIsPossible :: GameState -> Direction -> Maybe (Int,Int)
moveIsPossible gs dir = let
  (x,y) = getPlayerPosition gs
  (xOff,yOff) =
   case dir of
     North -> (1,0)
     South -> (-1,0)
     East -> (0,1)
     West -> (0,-1)
  desiredPosition = (x+xOff,y+yOff)
  b = gameBoard $ level gs
  in
  case get desiredPosition b of
   Nothing -> Nothing
   Just Wall -> Nothing
   Just GhostExit -> Nothing
   Just _ -> Just desiredPosition

getPlayerPosition :: GameState -> (Int, Int)
getPlayerPosition = tilePosition . player