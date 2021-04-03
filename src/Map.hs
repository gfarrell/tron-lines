module Map (
  tileMaxAge,
  Coord,
  Vec2D (..),
  Tile (..),
  Map,
  getCoordinates,
  getNextTileVec2D,
  getLifetime,
  moveHead,
  ageTile,
  getSurroundingTiles
) where

tileMaxAge :: Int
tileMaxAge = 4

type Coord = Int
data Vec2D = Vec2D Coord Coord
  deriving Show
instance Eq Vec2D where
  (==) (Vec2D x1 y1) (Vec2D x2 y2) = x1 == x2 && y1 == y2
  (/=) (Vec2D x1 y1) (Vec2D x2 y2) = x1 /= x2 || y1 /= y2

vecadd :: Vec2D -> Vec2D -> Vec2D
vecadd (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
vecsub :: Vec2D -> Vec2D -> Vec2D
vecsub (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)
len :: Vec2D -> Double
len (Vec2D x y) = sqrt (fromIntegral(x ^ 2) + fromIntegral(y ^ 2))

data Tile = Head Vec2D (Maybe Vec2D)
          | Tail Vec2D Int (Maybe Vec2D)
          | Empty Vec2D
  deriving Show

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

instance Eq Tile where
  (==) a b = getCoordinates a == getCoordinates b
  (/=) a b = getCoordinates a /= getCoordinates b


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
