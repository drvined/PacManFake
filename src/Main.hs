module Main (main) where

import Graphics.Gloss.Interface.IO.Game

import Config
import Controller
import View

main :: IO ()
main = do
  app <- initialApp
  playIO (InWindow "Pac-Man" (windowWidth, windowHeight) windowPosition)
         black            -- background colour
         framesPerSecond  -- frames per second
         app              -- initial state: the level menu
         viewIO           -- view function
         inputIO          -- event function
         stepIO           -- step function
