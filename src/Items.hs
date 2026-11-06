-- | The bonus items: fruit (bonus points) and candy (negative points, the
--   twist of our version). Both appear on random free tiles.
module Items (freeTiles, spawnItems, eatItems, itemPoints, pickDistinct) where

import Data.List (partition)

import Config
import Maze
import Model
import Player (addScore)

-- | Empty floor tiles that nobody is standing on and that hold no item.
freeTiles :: GameState -> [Pos]
freeTiles gs = filter isFree (allPositions m)
  where
    m        = maze gs
    occupied = posOf (player gs) : map posOf (ghosts gs) ++ map posOf (items gs)
    isFree p = tileAt m p == Floor && p `notElem` occupied

-- | Place one fruit and one candy on two different random free tiles. The
--   random number is generated in IO and passed in, so this stays pure.
spawnItems :: Int -> GameState -> GameState
spawnItems rnd gs = gs { items = items gs ++ zipWith Item kinds spots }
  where
    kinds = [Fruit, Candy]
    spots = pickDistinct rnd (length kinds) (freeTiles gs)

-- | Pick @k@ different elements from a list using one (large) random number:
--   the remainder chooses an element, the quotient is used for the next pick.
pickDistinct :: Int -> Int -> [a] -> [a]
pickDistinct rnd k xs
  | k <= 0 || null xs = []
  | otherwise = case splitAt (rnd `mod` n) xs of
      (before, x : after) -> x : pickDistinct (rnd `div` n) (k - 1) (before ++ after)
      (before, [])        -> before  -- cannot happen: the index is below n
  where n = length xs

-- | Pac-Man eats the items on his tile.
eatItems :: GameState -> GameState
eatItems gs = gs { items = remaining, player = addScore gained (player gs) }
  where
    (eaten, remaining) = partition (sameTile (player gs)) (items gs)
    gained             = sum (map (itemPoints . itemKind) eaten)

itemPoints :: ItemKind -> Int
itemPoints Fruit = fruitPoints
itemPoints Candy = candyPoints
