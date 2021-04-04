module Main where

import Map (Map, printMap, Tile (..), Vec2D (..))
import Simulation (nextFrameR)
import Render.Terminal as RT
import Control.Monad (when)

trialMap = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0), Empty (Vec2D 3 0), Empty (Vec2D 4 0)
           , Empty (Vec2D 0 1), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 4 Nothing, Empty (Vec2D 3 1), Empty (Vec2D 4 1)
           , Empty (Vec2D 0 2), Empty (Vec2D 1 2), Empty (Vec2D 2 2),  Empty (Vec2D 3 2), Empty (Vec2D 4 2)
           , Empty (Vec2D 0 3), Empty (Vec2D 1 3), Head (Vec2D 2 3) Nothing, Empty (Vec2D 3 3), Empty (Vec2D 4 3)
           , Empty (Vec2D 0 4), Empty (Vec2D 1 4), Empty (Vec2D 2 4), Empty (Vec2D 3 4), Empty (Vec2D 4 4) ]

printFrames :: [Map] -> IO ()
printFrames [] = return ()
printFrames (m:ms) = do
  putStr . printMap $ m
  printFrames ms

-- The first part of the programme generates the starting map. For the
-- time being we are just going to use the small trial map. In future we
-- will either read a map from a file or generate one randomly (hence
-- the IO wrapping).
getFirstMap :: IO Map
getFirstMap = return trialMap

-- Keep generating frames until the user asks you to stop by typing "q"
infiniteSimulationWithUserConsent :: Map -> IO ()
infiniteSimulationWithUserConsent m = do
  m' <- nextFrameR m
  render_ m'
  putStrLn "hit return to get the next frame, otherwise type q"
  res <- getLine
  when (res /= "q") . infiniteSimulationWithUserConsent $ m'

main :: IO ()
main = do
  m <- getFirstMap
  infiniteSimulationWithUserConsent m
