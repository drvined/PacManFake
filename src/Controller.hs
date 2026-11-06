-- | Connects the pure game to Gloss. These are the only game functions in IO:
--   they generate random numbers and read and write files, and then call the
--   pure functions from "Game" with the results.
module Controller (initialApp, stepIO, inputIO, keyCommand) where

import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)
import System.Random (randomRIO)

import Config
import FileIO
import Game
import Model

-- | Start in the level menu with the level list and high scores from disk.
initialApp :: IO App
initialApp = do
  files  <- listLevels levelsDirectory
  scores <- loadHighScores highScoreFile
  return App { screen = LevelMenu Nothing, levelFiles = files, highScores = scores }

-- | Step function for 'playIO'.
stepIO :: Float -> App -> IO App
stepIO dt app = case screen app of
  LevelMenu _ -> return app
  InGame gs   -> do
    rnd <- randomRIO (0, maxBound)
    let gs' = stepGame dt rnd gs
    -- Save the score exactly once: on the step in which the level ends.
    scores <- if not (isFinished gs) && isFinished gs'
                then recordScore gs' (highScores app)
                else return (highScores app)
    return app { screen = InGame gs', highScores = scores }

recordScore :: GameState -> [HighScore] -> IO [HighScore]
recordScore gs scores = do
  let entry = (levelName gs, score (player gs))
  saveHighScore highScoreFile entry
  return (entry : scores)

-- | Event function for 'playIO'.
--   Esc quits. Gloss tries to handle Esc itself, but that does not work with
--   the GLUT that comes with macOS, so we exit the program ourselves.
inputIO :: Event -> App -> IO App
inputIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
inputIO (EventKey key Down _ _) app = case screen app of
  LevelMenu _ -> menuKey key app
  InGame gs   -> return (gameKey key gs app)
inputIO _ app = return app

-- | In the menu a number key starts the level with that number.
menuKey :: Key -> App -> IO App
menuKey (Char c) app = case lookup c (zip levelKeys (levelFiles app)) of
  Just path -> startLevel path app
  Nothing   -> return app
menuKey _ app = return app

startLevel :: FilePath -> App -> IO App
startLevel path app = do
  result <- loadLevel path
  return app { screen = either failed started result }
  where
    name        = takeBaseName path
    failed err  = LevelMenu (Just ("Could not load " ++ name ++ ": " ++ err))
    started lvl = InGame (newGame name lvl)

-- | During a game: go back to the menu when paused or finished, otherwise
--   pass the command to the pure game logic.
gameKey :: Key -> GameState -> App -> App
gameKey key gs app
  | isMenuKey key && status gs /= Playing = app { screen = LevelMenu Nothing }
  | Just cmd <- keyCommand key            = app { screen = InGame (handleCommand cmd gs) }
  | otherwise                             = app

isMenuKey :: Key -> Bool
isMenuKey (Char 'm')            = True
isMenuKey (Char '\r')           = True
isMenuKey (SpecialKey KeyEnter) = True
isMenuKey _                     = False

-- | Translate a key to a game command: arrow keys or WASD steer, P pauses.
keyCommand :: Key -> Maybe Command
keyCommand key = case key of
  SpecialKey KeyUp    -> steer North
  SpecialKey KeyDown  -> steer South
  SpecialKey KeyLeft  -> steer West
  SpecialKey KeyRight -> steer East
  Char 'w'            -> steer North
  Char 's'            -> steer South
  Char 'a'            -> steer West
  Char 'd'            -> steer East
  Char 'p'            -> Just TogglePause
  _                   -> Nothing
  where steer = Just . Steer
