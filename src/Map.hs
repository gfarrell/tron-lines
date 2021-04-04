module Map (
  tileMaxAge,
  Coord,
  Vec2D (..),
  getX,
  getY,
  vecadd,
  vecsub,
  Tile (..),
  isEmpty,
  isHead,
  isTail,
  Map,
  getCoordinates,
  getNextTileVec2D,
  getLifetime,
  colocatedP,
  moveHead,
  ageTile,
  getSurroundingTiles,
  printMap
) where

tileMaxAge :: Int
tileMaxAge = 4

type Coord = Int
data Vec2D = Vec2D Coord Coord
  deriving Show
instance Eq Vec2D where
  (==) (Vec2D x1 y1) (Vec2D x2 y2) = x1 == x2 && y1 == y2
  (/=) (Vec2D x1 y1) (Vec2D x2 y2) = x1 /= x2 || y1 /= y2

getX :: Vec2D -> Coord
getX (Vec2D x _) = x
getY :: Vec2D -> Coord
getY (Vec2D _ y) = y
vecadd :: Vec2D -> Vec2D -> Vec2D
vecadd (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
vecsub :: Vec2D -> Vec2D -> Vec2D
vecsub (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)
len :: Vec2D -> Double
len (Vec2D x y) = sqrt (fromIntegral(x ^ 2) + fromIntegral(y ^ 2))

data Tile = Head Vec2D (Maybe Vec2D)
          | Tail Vec2D Int (Maybe Vec2D)
          | Empty Vec2D

instance Show Tile where
  show (Head (Vec2D x y) (Just (Vec2D a b))) = "[" ++ show x ++ "," ++ show y ++ " H  >" ++ show a ++ "," ++ show b ++ "]"
  show (Head (Vec2D x y) Nothing) = "[" ++ show x ++ "," ++ show y ++ " H  > ø ]"
  show (Tail (Vec2D x y) l (Just (Vec2D a b))) = "[" ++ show x ++ "," ++ show y ++ " T " ++ show l ++ ">" ++ show a ++ "," ++ show b ++ "]"
  show (Tail (Vec2D x y) l Nothing) = "[" ++ show x ++ "," ++ show y ++ " T " ++ show l ++ "> ø ]"
  show (Empty (Vec2D x y)) = "[" ++ show x ++ "," ++ show y ++ " E      ]"

isEmpty :: Tile -> Bool
isEmpty Empty {} = True
isEmpty _        = False

isHead :: Tile -> Bool
isHead Head {} = True
isHead _       = False

isTail :: Tile -> Bool
isTail Tail {} = True
isTail _       = False

type Map = [Tile]

getCoordinates :: Tile -> Vec2D
getCoordinates (Head p _)   = p
getCoordinates (Tail p _ _) = p
getCoordinates (Empty p )   = p

getNextTileVec2D :: Tile -> Maybe Vec2D
getNextTileVec2D (Head _ n)   = n
getNextTileVec2D (Tail _ _ n) = n
getNextTileVec2D _            = Nothing

getLifetime :: Tile -> Int
getLifetime (Tail _ l _) = l
getLifetime _            = 0

colocatedP :: Tile -> Tile -> Bool
colocatedP a b = getCoordinates a == getCoordinates b

instance Eq Tile where
  (==) (Head p n) (Head q m) = p == q && n == m
  (==) (Tail p k n) (Tail q l m) = p == q && n == m && k == l
  (==) (Empty p) (Empty q) = p == q
  (==) _ _ = False
  (/=) a b = not (a == b)

printMap :: Map -> String
printMap tiles = go xmax tiles
                 where xmax = maximum . map (getX . getCoordinates) $ tiles
                       go :: Int -> Map -> String
                       go l (t:ts) = show t ++ (if getX (getCoordinates t) >= l then "\n" else " ") ++ go l ts
                       go _ [] = "\n"

makeHead :: Vec2D -> Vec2D -> Tile
makeHead tileVec2D nextTileVec2D = Head tileVec2D (Just nextTileVec2D)
makeTail :: Tile -> Tile
makeTail tile = Tail (getCoordinates tile) tileMaxAge (getNextTileVec2D tile)

moveHead :: Map -> Tile -> Tile -> Map
moveHead (th:ts) headTile target
 | th == headTile = makeTail th : moveHead ts headTile target
 | th == target   = makeHead (getCoordinates target) (getCoordinates headTile) : moveHead ts headTile target
 | otherwise      = th : moveHead ts headTile target
moveHead ts _ _   = ts

ageTile :: Tile -> Tile
ageTile (Tail p l n) | l > 1     = Tail p (l - 1) n
                     | otherwise = Empty p
ageTile t                        = t

isAdjacent :: Tile -> Tile -> Bool
isAdjacent a b = (abs . len . vecsub (getCoordinates a) $ getCoordinates b) == 1

getSurroundingTiles :: Tile -> Map -> [Tile]
getSurroundingTiles centre = filter (isAdjacent centre)
