-- | Steering, moving and scoring of Pac-Man.
module Player (turn, movePlayer, addScore, resetPlayer) where

import Maze
import Model

-- | Remember the direction the player wants to go. Pac-Man turns as soon as
--   there is no wall in that direction, so a key may be pressed a bit early.
turn :: Direction -> GameState -> GameState
turn d gs = gs { player = (player gs) { nextDir = d } }

-- | Move Pac-Man one tile: in the wanted direction if possible, otherwise
--   straight on, otherwise he stands still against the wall.
movePlayer :: GameState -> GameState
movePlayer gs = gs { player = moved }
  where
    m       = maze gs
    p       = player gs
    ahead d = neighbour m d (playerPos p)
    moved   = case filter (isWalkable m . ahead) [nextDir p, playerDir p] of
      (d : _) -> p { playerDir = d, playerPos = ahead d }
      []      -> p

-- | Add (or, for candy, subtract) points. The score never drops below zero.
addScore :: Int -> Player -> Player
addScore points p = p { score = max 0 (score p + points) }

-- | Put Pac-Man back on his start tile, e.g. after losing a life.
resetPlayer :: Pos -> Player -> Player
resetPlayer home p = p { playerPos = home, playerDir = West, nextDir = West }
