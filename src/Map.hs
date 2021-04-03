module Map (
  tileMaxAge,
  Coord,
  Point (..),
  Tile (..),
  Map,
  getCoordinates,
  getNextTilePoint,
  getLifetime,
  moveHead,
  ageTile
) where

tileMaxAge :: Int
tileMaxAge = 4

type Coord = Int
data Point = Point Coord Coord
  deriving Show

instance Eq Point where
  (==) (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2
  (/=) (Point x1 y1) (Point x2 y2) = x1 /= x2 || y1 /= y2

data Tile = Head Point (Maybe Point)
          | Tail Point Int (Maybe Point)
          | Empty Point
  deriving Show

type Map = [Tile]

getCoordinates :: Tile -> Point
getCoordinates (Head p _)   = p
getCoordinates (Tail p _ _) = p
getCoordinates (Empty p )   = p

getNextTilePoint :: Tile -> Maybe Point
getNextTilePoint (Head _ n)   = n
getNextTilePoint (Tail _ _ n) = n
getNextTilePoint _            = Nothing

getLifetime :: Tile -> Int
getLifetime (Tail _ l _) = l
getLifetime _            = 0

instance Eq Tile where
  (==) a b = getCoordinates a == getCoordinates b
  (/=) a b = getCoordinates a /= getCoordinates b


makeHead :: Point -> Point -> Tile
makeHead tilePoint nextTilePoint = Head tilePoint (Just nextTilePoint)
makeTail :: Tile -> Tile
makeTail tile = Tail (getCoordinates tile) tileMaxAge (getNextTilePoint tile)

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
