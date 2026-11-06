-- | Pure functions to query and change the maze and to work with
--   positions and directions.
module Maze
  ( mazeWidth, mazeHeight
  , wrapPos, tileAt, setTile, isWalkable
  , directionVector, offset, neighbour, opposite
  , allPositions, indexedTiles
  , isPelletTile, pelletsLeft
  , distanceSq
  ) where

import Model

mazeHeight :: Maze -> Int
mazeHeight = length

mazeWidth :: Maze -> Int
mazeWidth []      = 0
mazeWidth (r : _) = length r

-- | Positions outside the maze wrap around to the other side. This is what
--   makes the tunnels on the left and right edge of a level work.
wrapPos :: Maze -> Pos -> Pos
wrapPos m (Pos c r) = Pos (c `mod` mazeWidth m) (r `mod` mazeHeight m)

tileAt :: Maze -> Pos -> Tile
tileAt m p = (m !! r) !! c
  where Pos c r = wrapPos m p

setTile :: Pos -> Tile -> Maze -> Maze
setTile (Pos c r) t = updateAt r (updateAt c (const t))

-- | Apply a function to the element at one index of a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f = zipWith change [0 ..]
  where change j x | j == i    = f x
                   | otherwise = x

isWalkable :: Maze -> Pos -> Bool
isWalkable m p = tileAt m p /= Wall

-- | (column change, row change) of one step in a direction. Rows grow
--   downwards, so North decreases the row.
directionVector :: Direction -> (Int, Int)
directionVector North = (0, -1)
directionVector South = (0, 1)
directionVector East  = (1, 0)
directionVector West  = (-1, 0)

-- | The position @n@ tiles further in a direction, without wrapping. Used for
--   ghost targets, which may lie outside the maze.
offset :: Int -> Direction -> Pos -> Pos
offset n d (Pos c r) = Pos (c + n * dc) (r + n * dr)
  where (dc, dr) = directionVector d

-- | The neighbouring tile in a direction (wrapping around the edges).
neighbour :: Maze -> Direction -> Pos -> Pos
neighbour m d = wrapPos m . offset 1 d

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East  = West
opposite West  = East

-- | All positions, row by row, in the same order as @concat maze@.
allPositions :: Maze -> [Pos]
allPositions m = [Pos c r | r <- [0 .. mazeHeight m - 1], c <- [0 .. mazeWidth m - 1]]

-- | Every tile together with its position.
indexedTiles :: Maze -> [(Pos, Tile)]
indexedTiles m = zip (allPositions m) (concat m)

isPelletTile :: Tile -> Bool
isPelletTile t = t == Pellet || t == PowerPellet

-- | Pellets (including power pellets) that are still in the maze.
pelletsLeft :: Maze -> Int
pelletsLeft = length . filter isPelletTile . concat

-- | Squared straight-line distance. Squaring keeps it an 'Int' and does not
--   change which of two distances is smaller.
distanceSq :: Pos -> Pos -> Int
distanceSq (Pos c1 r1) (Pos c2 r2) = sq (c1 - c2) + sq (r1 - r2)
  where sq x = x * x
