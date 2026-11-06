-- | Everything that touches the file system: finding and reading level files
--   and reading and writing the high score file.
module FileIO
  ( listLevels, loadLevel
  , loadHighScores, saveHighScore, parseHighScores
  ) where

import Control.Exception (IOException, try)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

import Config (levelExtension)
import Level (parseLevel)
import Model

-- | All level files in a folder, sorted by name.
listLevels :: FilePath -> IO [FilePath]
listLevels dir = do
  exists <- doesDirectoryExist dir
  if exists
    then map (dir </>) . sort . filter ((== levelExtension) . takeExtension)
           <$> listDirectory dir
    else return []

-- | Read and parse a level file. Problems are returned as a message for the
--   menu instead of crashing the game.
loadLevel :: FilePath -> IO (Either String Level)
loadLevel path = do
  result <- try (readFileStrict path)
  return $ case result of
    Left err       -> Left (show (err :: IOException))
    Right contents -> parseLevel contents

-- | Read the high score file; a missing file means no high scores yet.
loadHighScores :: FilePath -> IO [HighScore]
loadHighScores path = do
  exists <- doesFileExist path
  if exists
    then parseHighScores <$> readFileStrict path
    else return []

-- | Append one line @name score@ to the high score file.
saveHighScore :: FilePath -> HighScore -> IO ()
saveHighScore path (name, points) = appendFile path (name ++ " " ++ show points ++ "\n")

-- | Pure parser for the high score file. The last word on a line is the
--   score, the words before it are the name. Broken lines are skipped.
parseHighScores :: String -> [HighScore]
parseHighScores = mapMaybe parseLine . lines
  where
    parseLine line = case reverse (words line) of
      (points : nameParts@(_ : _)) ->
        fmap (\p -> (unwords (reverse nameParts), p)) (readMaybe points)
      _ -> Nothing

-- | 'readFile' is lazy and keeps the file open until everything is read.
--   We force the whole contents, so the file is closed before we append to it.
readFileStrict :: FilePath -> IO String
readFileStrict path = do
  contents <- readFile path
  length contents `seq` return contents
