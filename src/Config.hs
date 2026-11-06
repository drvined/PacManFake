-- | All tunable numbers of the game, collected in one place so that the
--   other modules contain no magic numbers.
module Config where

-- * Timing

-- | How many frames per second Gloss draws (and calls the step function).
framesPerSecond :: Int
framesPerSecond = 60

-- | Seconds between two game steps. In one step every character moves at
--   most one tile.
stepDuration :: Float
stepDuration = 0.15

-- | How long ghosts keep evading after Pac-Man ate a power pellet (seconds).
evadeDuration :: Float
evadeDuration = 6

-- | An evading ghost with fewer seconds left than this starts to flash.
evadeWarningTime :: Float
evadeWarningTime = 2

-- | Evading ghosts are slower: they only move on every n-th step.
evadingSlowdown :: Int
evadingSlowdown = 2

-- | Length of the death animation (seconds).
deathDuration :: Float
deathDuration = 1.5

-- * Rules

-- | Number of lives Pac-Man starts with.
startLives :: Int
startLives = 3

-- | A fruit and a candy appear when this many pellets have been eaten.
itemThresholds :: [Int]
itemThresholds = [70, 170]

-- | Pinky aims this many tiles in front of Pac-Man.
pinkyLookAhead :: Int
pinkyLookAhead = 4

-- | Inky mirrors Blinky through the tile this many tiles in front of Pac-Man.
inkyLookAhead :: Int
inkyLookAhead = 2

-- | Clyde only chases Pac-Man when he is further away than this (in tiles).
clydeShyDistance :: Int
clydeShyDistance = 8

-- * Points

pelletPoints, powerPelletPoints, ghostPoints, fruitPoints, candyPoints :: Int
pelletPoints      = 10
powerPelletPoints = 50
ghostPoints       = 200
fruitPoints       = 500
candyPoints       = -300   -- the twist: candy costs points

-- * Files

-- | Folder with the level files, relative to where the game is started.
levelsDirectory :: FilePath
levelsDirectory = "levels"

-- | Only files with this extension in 'levelsDirectory' are levels.
levelExtension :: String
levelExtension = ".txt"

-- | File the high scores are read from and appended to.
highScoreFile :: FilePath
highScoreFile = "highscores.txt"

-- | Keys used to choose a level in the menu (so at most nine levels).
levelKeys :: String
levelKeys = ['1' .. '9']

-- * Window and drawing

windowWidth, windowHeight :: Int
windowWidth  = 640
windowHeight = 760

-- | Position of the top left corner of the window on the screen.
windowPosition :: (Int, Int)
windowPosition = (100, 50)

-- | Size of one tile in pixels, before the board is scaled to fit the window.
tileSize :: Float
tileSize = 20

-- | Height of the score bar at the top of the window.
hudHeight :: Float
hudHeight = 60

-- | Small levels are scaled up, but at most by this factor.
maxBoardScale :: Float
maxBoardScale = 1.6
