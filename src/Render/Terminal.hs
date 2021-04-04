module Render.Terminal (render_) where

import System.Console.ANSI
import System.IO (hFlush, stdout)
import Map (
  Map,
  Tile (..),
  getCoordinates,
  getX,
  vecsub,
  isEmpty,
  isHead,
  isTail
  )

renderTile :: Tile -> String
renderTile Head {} = "⊙"
renderTile (Tail p l (Just n)) = if (abs . getX . vecsub p $ n) == 1 then "⎯" else "⎹"
renderTile (Tail p l Nothing) = "×"
renderTile Empty {} = "·"

renderMap :: Map -> String
renderMap tiles = render' xmax tiles
                  where xmax = maximum . map (getX . getCoordinates) $ tiles
                        render' :: Int -> Map -> String
                        render' l (t:ts) = renderTile t ++ (if (getX . getCoordinates $ t) >= l then "\n" else " ") ++ render' l ts
                        render' _ [] = ""

render_ :: Map -> IO ()
render_ [] = return ()
render_ tiles = do
  setSGR [Reset]
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Green]
  hFlush stdout
  putStr . renderMap $ tiles
  setSGR [Reset]
