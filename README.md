# Pac-Man (with candy)

Final project for Functional Programming by Matthieu Meylemans and Aidan Wetsteijn.

A Pac-Man remake in Haskell with Gloss. The twist: after 70 and 170 pellets a
**fruit** (+500 points) and a **candy** (-300 points) appear on random free
tiles. To get the highest score you want the fruit, not the candy.

## Building and running

From this folder (the one with `pacman.cabal`):

```
cabal build
cabal run pacman
```

Run the game from this folder, because it reads `levels/` and writes
`highscores.txt` relative to the current directory.

On Windows, Gloss needs `freeglut.dll`, which is included in this folder
(it comes from the course's example project). On macOS it is not needed.

## How to play

| Key                 | Action                                     |
|---------------------|--------------------------------------------|
| `1` – `9`           | choose a level in the menu                 |
| arrow keys / `WASD` | steer Pac-Man (you can press a turn early) |
| `P`                 | pause / continue                           |
| `M` or `Enter`      | back to the menu (when paused or finished) |
| `Esc`               | quit                                       |

Eat all pellets to clear the level. You have 3 lives.

| Thing         | Points |
|---------------|--------|
| pellet        | 10     |
| power pellet  | 50     |
| evading ghost | 200    |
| fruit         | 500    |
| candy         | -300   |

The score never drops below 0. All of these numbers live in `src/Config.hs`.

### The ghosts

All ghosts use the same steering: they never turn around (except in a dead
end) and at every crossing take the direction that brings them closest to
their target. Only the target differs:

* **Blinky** (red) aims at Pac-Man.
* **Pinky** (pink) aims four tiles in front of Pac-Man.
* **Inky** (cyan) works with Blinky: he takes the tile two in front of
  Pac-Man and mirrors Blinky's position through it, so the two trap Pac-Man
  from opposite sides.
* **Clyde** (orange) chases Pac-Man when he is more than 8 tiles away, and
  retreats to the bottom left corner when he gets closer.

After a power pellet the ghosts turn blue for 6 seconds: they turn around, run
away from Pac-Man at half speed and can be eaten. They flash white when the
time is almost up. An eaten ghost goes back to its start tile.

## Levels (and making your own)

Every `.txt` file in `levels/` shows up in the menu (sorted by name, at most
nine). One character per tile:

| Char    | Meaning                                                  |
|---------|----------------------------------------------------------|
| `#`     | wall                                                     |
| `.`     | pellet                                                   |
| `o`     | power pellet                                             |
| (space) | empty floor                                              |
| `P`     | Pac-Man's start tile                                     |
| `G`     | ghost start tile (1 to 4; reused if fewer than 4)        |

A row whose leftmost and rightmost tiles are open is a tunnel to the other
side. Shorter rows are filled up with walls, so keep trailing spaces in tunnel
rows. If a level file is broken, the menu shows why instead of crashing.

## High scores

When a level ends (game over or cleared), a line `levelname score` is appended
to `highscores.txt`. The file is read at start-up and the menu shows the best
score per level.

## Code overview

| Module       | Contents                                                       | Pure? |
|--------------|----------------------------------------------------------------|-------|
| `Config`     | all constants (timing, points, sizes, file names)              | yes   |
| `Model`      | data types, `HasPos` type class, `newGame`                     | yes   |
| `Maze`       | tile lookup and update, directions, wrapping                   | yes   |
| `Level`      | `parseLevel`: level text to a `Level`                          | yes   |
| `Player`     | `turn`, `movePlayer`, `addScore`                               | yes   |
| `Ghost`      | `targetOf`, `moveGhost`, `tickGhostMode`, `frighten`           | yes   |
| `Items`      | `freeTiles`, `spawnItems`, `eatItems`                          | yes   |
| `Game`       | `stepGame`, `handleCommand`, collisions, death animations      | yes   |
| `View`       | `render`, menu, `Drawable` type class                          | yes   |
| `FileIO`     | `listLevels`, `loadLevel`, `loadHighScores`, `saveHighScore`   | IO    |
| `Controller` | `stepIO`, `inputIO`: random numbers, files, key mapping        | IO    |
| `Main`       | `playIO`                                                       | IO    |

## Changes compared to the design document

* **Extra fields.** `Player` got `nextDir` (the direction the player asked
  for), so a turn can be pressed a bit before the corner. `Ghost` got
  `ghostHome` (where it returns after being eaten). `GameState` got
  `playerHome`, `stepTimer` and `stepCount`: Gloss runs at 60 frames per
  second, but characters move one tile every 0.15 s.
* **`Status` has `Won`** for a cleared level.
* **App and Screen.** Because of the level menu, `playIO` works on an `App`
  (`screen`, `levelFiles`, `highScores`) instead of directly on `GameState`.
  `Screen` is either `LevelMenu` or `InGame GameState`.
* **Commands instead of keys.** The pure logic gets a `Command`
  (`Steer Direction` or `TogglePause`) via `handleCommand`, instead of
  `handleKey :: Char -> GameState -> GameState`. Arrow keys are not `Char`s
  in Gloss, and this keeps the game logic independent of Gloss.
* **`targetOf :: Ghost -> GameState -> Pos`** takes the whole ghost, because
  Clyde needs his own position.
* **Inky no longer guards a fixed corner.** A ghost that only circles a
  corner hardly tries to catch Pac-Man, so Inky now uses the original game's
  rule: mirror Blinky through the tile two in front of Pac-Man.
* **Level files** also have `o` (power pellet), `P` and `G` (start tiles).
  `loadLevel` returns `IO (Either String Level)` so broken files give a
  message. The parsing itself is the pure `parseLevel`.
* **Evading ghosts** run away from Pac-Man (they take the direction that
  maximises their distance to him) at half speed.
* **Type classes.** `HasPos` (everything on a tile) is used for collisions and
  item eating; `Drawable` (in `View`) draws every kind of thing on its own tile.
* **Points** follow the original game (pellet 10 instead of 1).
* Function names are lower case (`eatPowerPellet`, `tickGhostMode`), as
  Haskell requires.
