module MapSpec (spec) where

import Test.Hspec
import Map

isEmpty :: Tile -> Bool
isEmpty (Empty _) = True
isEmpty _         = False

isHead :: Tile -> Bool
isHead (Head _ _) = True
isHead _          = False

isTail :: Tile -> Bool
isTail (Tail _ _ _) = True
isTail _            = False


spec :: Spec
spec = do
  describe "tiles" $ do
    it "are equal if both x and y coordinates are the same" $ do
      let a = Head (Point 15 26) Nothing
          b = Tail (Point 15 26) 1 Nothing
          c = Empty (Point 15 26)
      shouldBe (a == b) True
      shouldBe (a == c) True
      shouldBe (c == b) True

    it "are not equal if either x coordinates or y coordinates do not match" $ do
      let a = Empty (Point 15 26)
          b = Empty (Point 10 26)
          c = Empty (Point 15 42)
          d = Empty (Point 10 42)
      shouldBe (a /= b) True
      shouldBe (a /= c) True
      shouldBe (a /= d) True

  describe "moveHead" $
    it "returns a Map for which the given Head tile has been correctly moved" $ do
      let map = [ Empty (Point 0 0), Empty (Point 1 0), Empty (Point 2 0),
                  Empty (Point 0 1), Head (Point 1 1) Nothing, Empty (Point 2 1),
                  Empty (Point 0 2), Empty (Point 1 2), Empty (Point 2 2) ]
          exp = [ Empty (Point 0 0), Empty (Point 1 0), Empty (Point 2 0),
                  Empty (Point 0 1), Tail (Point 1 1) tileMaxAge Nothing, Empty (Point 2 1),
                  Empty (Point 0 2), Head (Point 1 2) (Just (Point 1 1)), Empty (Point 2 2) ]
      moveHead map (Head (Point 1 1) Nothing) (Empty (Point 1 2)) `shouldBe` exp

  describe "ageTile" $ do
    it "reduces a Tail tile's age by one" $ do
      let result = ageTile (Tail (Point 1 1) 3 Nothing)
      getLifetime result `shouldBe` 2
      getCoordinates result `shouldBe` Point 1 1
      getNextTilePoint result `shouldBe` Nothing

    it "returns an Empty tile if the Tail tile's age is 1 or less" $ do
      let result = ageTile . Tail (Point 1 1) 1 $ Nothing
      isEmpty result `shouldBe` True
      getCoordinates result `shouldBe` Point 1 1

    it "returns the same tile if given a Head tile" $ do
      let result = ageTile . Head (Point 1 1) . Just $ Point 2 2
      getCoordinates result `shouldBe` Point 1 1
      getNextTilePoint result `shouldBe` Just (Point 2 2)
      isHead result `shouldBe` True

    it "results the same tile if given an Empty tile" $ do
      let result = ageTile . Empty $ Point 1 1
      isEmpty result `shouldBe` True
      getCoordinates result `shouldBe` Point 1 1
