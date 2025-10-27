-- All the relevant types
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
module Model where
import GHC.Generics (Generic)
import Utils.Count (Timer, liveCounter, LiveCounter, ScoreCounter, RoundCounter, roundCounter, timeCounter, scoreCounter)
import Utils.Board
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key)
import Text.Printf (vFmt)

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
  { spawnPosition = (13.5, 14)
  , gameBoard = realBoard
  , ghosts = []
  }

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
move gs dir = (player gs) {position = pos}
  where
    pos =
     case moveIsPossible gs dir of
      Nothing -> getPlayerPosition gs
      Just a -> a   

moveIsPossible :: GameState -> Direction -> Maybe (Float,Float)
moveIsPossible gs dir = let
  (x,y) = getPlayerPosition gs
  (xOff,yOff) =
   case dir of
     North -> (1,0)
     South -> (-1,0)
     East -> (0,1)
     West -> (0,-1)
  (desiredX,desiredY) = (x+xOff*0.125,y+yOff*0.125)
  tileToCheck =
    case dir of 
      North -> (floor (desiredX + 0.5), floor desiredY)
      South -> (floor (desiredX - 0.5), floor desiredY)
      East  -> (floor desiredX, floor (desiredY + 0.5))
      West  -> (floor desiredX, floor (desiredY - 0.5))
  b = gameBoard $ level gs
  in
  case get tileToCheck b of
   Nothing -> Nothing
   Just Wall -> Nothing
   Just GhostExit -> Nothing
   Just _ -> 
    case dir of 
      North -> Just (desiredX, fromInteger (floor desiredY) + 0.5)
      South -> Just (desiredX, fromInteger (floor desiredY) + 0.5)
      East  -> Just (fromInteger (floor desiredX) + 0.5,desiredY)
      West  -> Just (fromInteger (floor desiredX) + 0.5,desiredY) 

-- if up and row 

getPlayerPosition :: GameState -> (Float, Float)
getPlayerPosition = position . player