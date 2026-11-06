-- | Parsing level files. This is pure: 'FileIO.loadLevel' reads the file and
--   hands the text to 'parseLevel'.
--
--   The level format, one character per tile:
--
--   > #  wall
--   > .  pellet
--   > o  power pellet
--   >    (space) empty floor
--   > P  Pac-Man's start tile (exactly one is used)
--   > G  a ghost's start tile (at least one; Blinky, Pinky, Inky and Clyde
--   >    take them in that order, reusing them if there are fewer than four)
--
--   Rows that are shorter than the longest row are filled up with walls. A row
--   whose first and last tile are open forms a tunnel to the other side.
module Level (parseLevel) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Maze (pelletsLeft)
import Model

parseLevel :: String -> Either String Level
parseLevel input
  | null rows = Left "the level file is empty"
  | otherwise = do
      tiles       <- traverse (traverse charToTile) grid
      start       <- firstOr "there is no Pac-Man start ('P')" (positionsOf 'P')
      ghostStarts <- atLeastOne "there is no ghost start ('G')" (positionsOf 'G')
      if pelletsLeft tiles == 0
        then Left "the level has no pellets"
        else Right (Level tiles start ghostStarts)
  where
    -- Ignore Windows line endings and empty lines at the end of the file.
    rows  = dropWhileEnd (all isSpace) (map (filter (/= '\r')) (lines input))
    width = maximum (map length rows)
    grid  = map (padTo width) rows
    positionsOf ch = [ Pos c r | (r, line) <- zip [0 ..] grid
                               , (c, x)    <- zip [0 ..] line
                               , x == ch ]

padTo :: Int -> String -> String
padTo w s = s ++ replicate (w - length s) '#'

charToTile :: Char -> Either String Tile
charToTile c = case c of
  '#' -> Right Wall
  '.' -> Right Pellet
  'o' -> Right PowerPellet
  ' ' -> Right Floor
  'P' -> Right Floor
  'G' -> Right Floor
  _   -> Left ("unknown character " ++ show c)

firstOr :: String -> [a] -> Either String a
firstOr err []      = Left err
firstOr _   (x : _) = Right x

atLeastOne :: String -> [a] -> Either String [a]
atLeastOne err [] = Left err
atLeastOne _   xs = Right xs
