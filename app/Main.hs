module Main where

import Map (Map, printMap, Tile (..), Vec2D (..))
import Simulation (runFramesR)

trialMap = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
           , Empty (Vec2D 0 1), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 4 Nothing
           , Empty (Vec2D 0 2), Empty (Vec2D 1 2), Empty (Vec2D 2 2) ]

printFrames :: [Map] -> IO ()
printFrames [] = return ()
printFrames (m:ms) = do
  putStr . printMap $ m
  printFrames ms

main :: IO ()
main = do
  putStrLn "Running simulation of trial map 5 times"
  putStr . printMap $ trialMap
  putStrLn "..."
  runFramesR 5 trialMap >>= printFrames
