-- | The data types that describe the game, plus a few small helpers to
--   build and query them. Every kind of thing has its own type (there is no
--   generic "object" type), so the compiler knows what a ghost is and what
--   the player is.
module Model where

import Config (startLives)

-- * Positions, directions and tiles

-- | A position in the maze, counted in tiles. Column 0 is the left edge and
--   row 0 the top edge, exactly as in the level files.
data Pos = Pos { col :: Int, row :: Int }
  deriving (Eq, Ord, Show)

data Direction = North | South | East | West
  deriving (Eq, Show, Enum, Bounded)

data Tile = Wall | Floor | Pellet | PowerPellet
  deriving (Eq, Show)

-- | The maze as a list of rows; the first row is the top of the screen.
type Maze = [[Tile]]

-- * Player, ghosts and items

data Player = Player
  { playerPos :: Pos
  , playerDir :: Direction  -- ^ the direction Pac-Man is moving in
  , nextDir   :: Direction  -- ^ the direction the player asked for; it is
                            --   taken as soon as there is no wall in the way
  , score     :: Int
  , lives     :: Int
  } deriving Show

data GhostKind = Blinky | Pinky | Inky | Clyde
  deriving (Eq, Show, Enum, Bounded)

-- | 'Evading' carries the number of seconds the ghost keeps evading.
data GhostMode = Chasing | Evading Float
  deriving (Eq, Show)

data Ghost = Ghost
  { ghostKind :: GhostKind
  , ghostPos  :: Pos
  , ghostDir  :: Direction
  , ghostMode :: GhostMode
  , ghostHome :: Pos  -- ^ start tile; the ghost goes back here when eaten
  } deriving Show

data ItemKind = Fruit | Candy
  deriving (Eq, Show)

data Item = Item { itemKind :: ItemKind, itemPos :: Pos }
  deriving Show

-- * Game state

data Status = Playing | Paused | GameOver | Won
  deriving (Eq, Show)

data DeathAnimation = DeathAnimation { animPos :: Pos, timeLeft :: Float }
  deriving Show

data GameState = GameState
  { maze            :: Maze
  , player          :: Player
  , playerHome      :: Pos    -- ^ where Pac-Man starts after losing a life
  , ghosts          :: [Ghost]
  , items           :: [Item]
  , pelletsEaten    :: Int
  , deathAnimations :: [DeathAnimation]
  , status          :: Status
  , levelName       :: String
  , stepTimer       :: Float  -- ^ seconds since the last game step
  , stepCount       :: Int    -- ^ number of game steps so far (drives animations)
  } deriving Show

-- | A level as it is read from a file, before the game starts.
data Level = Level
  { levelMaze        :: Maze
  , levelPlayerStart :: Pos
  , levelGhostStarts :: [Pos]  -- ^ never empty, see 'Level.parseLevel'
  } deriving Show

-- | What the player can ask the game to do. The controller turns key presses
--   into commands, so the game logic does not depend on Gloss.
data Command = Steer Direction | TogglePause
  deriving (Eq, Show)

-- | A line in the high score file: level name and score.
type HighScore = (String, Int)

-- * The whole application

-- | Which screen is shown. The menu may show an error message, e.g. when a
--   level file could not be read.
data Screen = LevelMenu (Maybe String)
            | InGame GameState

data App = App
  { screen     :: Screen
  , levelFiles :: [FilePath]
  , highScores :: [HighScore]
  }

-- * Things with a position

-- | Everything that stands on a tile of the maze.
class HasPos a where
  posOf :: a -> Pos

instance HasPos Player where
  posOf = playerPos

instance HasPos Ghost where
  posOf = ghostPos

instance HasPos Item where
  posOf = itemPos

instance HasPos DeathAnimation where
  posOf = animPos

-- | Do two things stand on the same tile?
sameTile :: (HasPos a, HasPos b) => a -> b -> Bool
sameTile a b = posOf a == posOf b

-- * Building and querying the state

-- | The state at the start of a level.
newGame :: String -> Level -> GameState
newGame name (Level m start ghostStarts) = GameState
  { maze            = m
  , player          = Player start West West 0 startLives
  , playerHome      = start
  , ghosts          = zipWith newGhost [minBound .. maxBound] (cycle homes)
  , items           = []
  , pelletsEaten    = 0
  , deathAnimations = []
  , status          = Playing
  , levelName       = name
  , stepTimer       = 0
  , stepCount       = 0
  }
  where
    -- 'cycle' needs a non-empty list; fall back to Pac-Man's start to be safe.
    homes | null ghostStarts = [start]
          | otherwise        = ghostStarts

newGhost :: GhostKind -> Pos -> Ghost
newGhost kind home = Ghost kind home North Chasing home

isEvading :: Ghost -> Bool
isEvading g = case ghostMode g of
  Evading _ -> True
  Chasing   -> False

-- | While a death animation plays the rest of the game is frozen.
dying :: GameState -> Bool
dying = not . null . deathAnimations

-- | Has the level ended (lost or won)?
isFinished :: GameState -> Bool
isFinished gs = status gs == GameOver || status gs == Won

-- | The best score ever reached on a level, if any.
bestScore :: String -> [HighScore] -> Maybe Int
bestScore name scores = case [points | (n, points) <- scores, n == name] of
  []     -> Nothing
  points -> Just (maximum points)
