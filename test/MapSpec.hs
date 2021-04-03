module MapSpec (spec) where

import Test.Hspec
import Map

isEmpty :: Tile -> Bool
isEmpty Empty {} = True
isEmpty _        = False

isHead :: Tile -> Bool
isHead Head {} = True
isHead _       = False

isTail :: Tile -> Bool
isTail Tail {} = True
isTail _       = False


spec :: Spec
spec = do
  describe "tiles" $ do
    it "are equal if both x and y coordinates are the same" $ do
      let a = Head (Vec2D 15 26) Nothing
          b = Tail (Vec2D 15 26) 1 Nothing
          c = Empty (Vec2D 15 26)
      shouldBe (a == b) True
      shouldBe (a == c) True
      shouldBe (c == b) True

    it "are not equal if either x coordinates or y coordinates do not match" $ do
      let a = Empty (Vec2D 15 26)
          b = Empty (Vec2D 10 26)
          c = Empty (Vec2D 15 42)
          d = Empty (Vec2D 10 42)
      shouldBe (a /= b) True
      shouldBe (a /= c) True
      shouldBe (a /= d) True

  describe "moveHead" $
    it "returns a Map for which the given Head tile has been correctly moved" $ do
      let map = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0),
                  Empty (Vec2D 0 1), Head (Vec2D 1 1) Nothing, Empty (Vec2D 2 1),
                  Empty (Vec2D 0 2), Empty (Vec2D 1 2), Empty (Vec2D 2 2) ]
          exp = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0),
                  Empty (Vec2D 0 1), Tail (Vec2D 1 1) tileMaxAge Nothing, Empty (Vec2D 2 1),
                  Empty (Vec2D 0 2), Head (Vec2D 1 2) (Just (Vec2D 1 1)), Empty (Vec2D 2 2) ]
      moveHead map (Head (Vec2D 1 1) Nothing) (Empty (Vec2D 1 2)) `shouldBe` exp

  describe "ageTile" $ do
    it "reduces a Tail tile's age by one" $ do
      let result = ageTile (Tail (Vec2D 1 1) 3 Nothing)
      getLifetime result `shouldBe` 2
      getCoordinates result `shouldBe` Vec2D 1 1
      getNextTileVec2D result `shouldBe` Nothing

    it "returns an Empty tile if the Tail tile's age is 1 or less" $ do
      let result = ageTile . Tail (Vec2D 1 1) 1 $ Nothing
      isEmpty result `shouldBe` True
      getCoordinates result `shouldBe` Vec2D 1 1

    it "returns the same tile if given a Head tile" $ do
      let result = ageTile . Head (Vec2D 1 1) . Just $ Vec2D 2 2
      getCoordinates result `shouldBe` Vec2D 1 1
      getNextTileVec2D result `shouldBe` Just (Vec2D 2 2)
      isHead result `shouldBe` True

    it "results the same tile if given an Empty tile" $ do
      let result = ageTile . Empty $ Vec2D 1 1
      isEmpty result `shouldBe` True
      getCoordinates result `shouldBe` Vec2D 1 1

  describe "getSurroundingTiles" $
    it "returns the tiles from the map which are adjacent to this one (not diagonals)" $ do
      let map = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0),
                  Empty (Vec2D 0 1), Head (Vec2D 1 1) Nothing, Empty (Vec2D 2 1),
                  Empty (Vec2D 0 2), Empty (Vec2D 1 2), Empty (Vec2D 2 2) ]
      getSurroundingTiles (Empty (Vec2D 0 2)) map `shouldBe` [ Empty (Vec2D 0 1),
                                                               Empty (Vec2D 1 2) ]
      getSurroundingTiles (Head (Vec2D 1 1) Nothing) map `shouldBe` [ Empty (Vec2D 1 0),
                                                                      Empty (Vec2D 0 1),
                                                                      Empty (Vec2D 2 1),
                                                                      Empty (Vec2D 1 2) ]
