-- | The pure game logic: advancing the game in time and reacting to the
--   player's commands. Nothing in here does IO; random numbers are passed in.
module Game
  ( stepGame, handleCommand
  , eatPellet, eatPowerPellet, resolveCollisions, stepDeathAnimations
  ) where

import Config
import Ghost
import Items
import Maze
import Model
import Player

-- | Advance the game by @dt@ seconds. The Int is a random number from IO,
--   used when items have to be placed.
--
--   Gloss calls this every frame. Timers (evading, animations) run every
--   frame, but the characters only move once every 'stepDuration' seconds.
stepGame :: Float -> Int -> GameState -> GameState
stepGame dt rnd gs
  | status gs /= Playing             = gs
  | dying gs                         = checkGameOver (stepDeathAnimations dt gs)
  | stepTimer ticked >= stepDuration = gameStep rnd (nextStep ticked)
  | otherwise                        = ticked
  where
    ticked     = gs { stepTimer = stepTimer gs + dt
                    , ghosts    = map (tickGhostMode dt) (ghosts gs) }
    nextStep s = s { stepTimer = stepTimer s - stepDuration
                   , stepCount = stepCount s + 1 }

-- | One game step: Pac-Man moves and eats, items may appear, the ghosts move.
--   Collisions are checked both before and after the ghosts move, so Pac-Man
--   and a ghost cannot walk through each other.
gameStep :: Int -> GameState -> GameState
gameStep rnd gs
  | pelletsLeft (maze fed) == 0 = fed { status = Won }
  | otherwise = (resolveCollisions . moveGhosts . resolveCollisions . maybeSpawn) fed
  where
    fed = eatItems (eatPellet (movePlayer gs))
    -- Items appear exactly when a threshold is reached, not on every step
    -- that Pac-Man happens to stand still afterwards.
    maybeSpawn s
      | pelletsEaten s /= pelletsEaten gs && pelletsEaten s `elem` itemThresholds
                  = spawnItems rnd s
      | otherwise = s

moveGhosts :: GameState -> GameState
moveGhosts gs
  | dying gs  = gs
  | otherwise = gs { ghosts = map moveIfAllowed (ghosts gs) }
  where
    moveIfAllowed g
      | movesThisStep (stepCount gs) g = moveGhost gs g
      | otherwise                      = g

-- | Eat the pellet on Pac-Man's tile, if there is one.
eatPellet :: GameState -> GameState
eatPellet gs = case tileAt (maze gs) here of
    Pellet      -> consume pelletPoints gs
    PowerPellet -> eatPowerPellet (consume powerPelletPoints gs)
    _           -> gs
  where
    here = playerPos (player gs)
    consume points s = s { maze         = setTile here Floor (maze s)
                         , pelletsEaten = pelletsEaten s + 1
                         , player       = addScore points (player s) }

-- | All ghosts start evading (and turn around).
eatPowerPellet :: GameState -> GameState
eatPowerPellet gs = gs { ghosts = map frighten (ghosts gs) }

-- | Pac-Man meets ghosts on his tile: a chasing ghost costs him a life,
--   evading ghosts are eaten for bonus points and sent home.
resolveCollisions :: GameState -> GameState
resolveCollisions gs
  | dying gs                   = gs
  | any (not . isEvading) hits = loseLife gs
  | otherwise = gs { ghosts = map eatIfHit (ghosts gs)
                   , player = addScore (ghostPoints * length hits) (player gs) }
  where
    hits = filter (sameTile (player gs)) (ghosts gs)
    eatIfHit g
      | sameTile (player gs) g = sendHome g
      | otherwise              = g

-- | Pac-Man was caught: one life less, start the death animation and put
--   everybody back on their start tiles.
loseLife :: GameState -> GameState
loseLife gs = gs
  { player          = resetPlayer (playerHome gs) (pac { lives = lives pac - 1 })
  , ghosts          = map sendHome (ghosts gs)
  , deathAnimations = DeathAnimation (playerPos pac) deathDuration : deathAnimations gs
  , stepTimer       = 0
  }
  where pac = player gs

-- | Count down all death animations and drop the finished ones.
stepDeathAnimations :: Float -> GameState -> GameState
stepDeathAnimations dt gs =
  gs { deathAnimations = filter ((> 0) . timeLeft) (map tick (deathAnimations gs)) }
  where tick a = a { timeLeft = timeLeft a - dt }

-- | After the last death animation the game is over if no lives are left.
checkGameOver :: GameState -> GameState
checkGameOver gs
  | not (dying gs) && lives (player gs) <= 0 = gs { status = GameOver }
  | otherwise                                = gs

handleCommand :: Command -> GameState -> GameState
handleCommand (Steer d) gs
  | status gs == Playing = turn d gs
  | otherwise            = gs
handleCommand TogglePause gs = gs { status = toggle (status gs) }
  where
    toggle Playing = Paused
    toggle Paused  = Playing
    toggle other   = other
