module Simulation (
  nextFrame,
  nextFrameR,
  runFramesR,
) where

import Map (
  Map,
  Tile (..),
  isEmpty,
  getSurroundingTiles,
  moveHead,
  colocatedP,
  ageTile)

import System.Random (RandomGen, newStdGen)
import System.Random.Shuffle (shuffle')

nextFrame :: RandomGen gen => gen -> Map -> Map
nextFrame gen tiles = go tiles tiles
  where go :: [Tile] -> Map -> Map
        go (t@Head {}:ts) state =
          let choices = filter isEmpty . getSurroundingTiles t $ state
          in go ts $ if null choices
                      then state
                      else moveHead state t . head . shuffle' choices (length choices) $ gen
        go (t@Tail {}:ts) state = go ts . map (\x -> if colocatedP x t then ageTile t else x) $ state
        go (t@Empty {}:ts) state = go ts state
        go [] state = state

iterateM :: Monad m => (a -> m a) -> Int -> a -> m [a]
iterateM f = iterateM'
  where iterateM' 0 _ = return []
        iterateM' n x = (x:) <$> (f x >>= iterateM' (pred n))

nextFrameR :: Map -> IO Map
nextFrameR tiles = do
  gen <- newStdGen
  return (nextFrame gen tiles)

runFramesR :: Int -> Map -> IO [Map]
runFramesR = iterateM nextFrameR
