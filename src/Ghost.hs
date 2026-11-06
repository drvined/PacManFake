-- | Ghost behaviour. All four ghosts are steered by the same function,
--   'moveGhost'; only their target ('targetOf') differs.
module Ghost
  ( targetOf, moveGhost, movesThisStep
  , tickGhostMode, reverseGhost, frighten, sendHome
  ) where

import Data.List (find, minimumBy)
import Data.Ord (comparing)

import Config
import Maze
import Model

-- | The tile a chasing ghost tries to reach. It is computed from the game
--   state every step instead of being stored.
--
--   * Blinky aims at Pac-Man himself.
--   * Pinky aims a few tiles in front of Pac-Man, to cut him off.
--   * Inky works together with Blinky: take the tile two steps in front of
--     Pac-Man and mirror Blinky's position through it. When Blinky is right
--     behind Pac-Man, Inky ends up in front of him, so the two trap Pac-Man
--     from both sides.
--   * Clyde chases Pac-Man when he is far away, but retreats to the bottom
--     left corner when he gets close.
targetOf :: Ghost -> GameState -> Pos
targetOf g gs = case ghostKind g of
    Blinky -> pacPos
    Pinky  -> offset pinkyLookAhead (playerDir pac) pacPos
    Inky   -> mirrorThrough (offset inkyLookAhead (playerDir pac) pacPos) blinkyPos
    Clyde
      | distanceSq (ghostPos g) pacPos > clydeShyDistance * clydeShyDistance -> pacPos
      | otherwise -> bottomLeft
  where
    pac        = player gs
    pacPos     = playerPos pac
    m          = maze gs
    bottomLeft = Pos 0 (mazeHeight m - 1)
    -- Without a Blinky in the game, Inky simply aims at Pac-Man.
    blinkyPos  = maybe pacPos ghostPos (find ((== Blinky) . ghostKind) (ghosts gs))

-- | @mirrorThrough centre p@ is the point on the other side of @centre@,
--   at the same distance as @p@.
mirrorThrough :: Pos -> Pos -> Pos
mirrorThrough (Pos cc cr) (Pos pc pr) = Pos (2 * cc - pc) (2 * cr - pr)

-- | Move a ghost one tile. Like in the original game a ghost does not turn
--   around (unless it is in a dead end); of the remaining open directions it
--   takes the one whose next tile is closest to its target. An evading ghost
--   instead takes the direction that gets it furthest away from Pac-Man.
moveGhost :: GameState -> Ghost -> Ghost
moveGhost gs g = case options of
    [] -> g
    _  -> let d = minimumBy (comparing preference) options
          in g { ghostDir = d, ghostPos = step d }
  where
    m       = maze gs
    step d  = neighbour m d (ghostPos g)
    -- The order of this list breaks ties, as in the original game.
    open    = filter (isWalkable m . step) [North, West, South, East]
    forward = filter (/= opposite (ghostDir g)) open
    options | null forward = open
            | otherwise    = forward

    -- Lower is better.
    preference :: Direction -> Int
    preference d = case ghostMode g of
      Chasing   -> distanceSq (step d) (targetOf g gs)
      Evading _ -> negate (distanceSq (step d) (playerPos (player gs)))

-- | Evading ghosts are slower than chasing ghosts: they skip some steps.
movesThisStep :: Int -> Ghost -> Bool
movesThisStep stepNr g = case ghostMode g of
  Chasing   -> True
  Evading _ -> stepNr `mod` evadingSlowdown == 0

-- | Count down the evading timer; when it runs out the ghost chases again.
tickGhostMode :: Float -> Ghost -> Ghost
tickGhostMode dt g = case ghostMode g of
  Evading t
    | t > dt    -> g { ghostMode = Evading (t - dt) }
    | otherwise -> g { ghostMode = Chasing }
  Chasing -> g

reverseGhost :: Ghost -> Ghost
reverseGhost g = g { ghostDir = opposite (ghostDir g) }

-- | What a power pellet does to a ghost: evade for a while and turn around.
frighten :: Ghost -> Ghost
frighten g = reverseGhost (g { ghostMode = Evading evadeDuration })

-- | Send a ghost back to its start tile, chasing again. Used when Pac-Man eats
--   it and when Pac-Man loses a life.
sendHome :: Ghost -> Ghost
sendHome g = g { ghostPos = ghostHome g, ghostDir = North, ghostMode = Chasing }
