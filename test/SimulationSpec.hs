module SimulationSpec (spec) where

import Test.Hspec
import System.Random (mkStdGen)
import Simulation
import Map (
  Vec2D (..),
  Tile (..),
  printMap
  )

import Debug.Trace

spec :: Spec
spec = do
  describe "nextFrame" $ do
    -- We use a fixed seed to make our tests deterministic
    let seed = mkStdGen 42
    it "moves a Head tile into a random space" $
      let map = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
                , Empty (Vec2D 0 1), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 4 Nothing
                , Empty (Vec2D 0 2), Head (Vec2D 1 2) Nothing, Empty (Vec2D 2 2) ]
          exp = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
                , Head (Vec2D 0 1) (Just (Vec2D 1 1)), Tail (Vec2D 1 1) 4 (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 3 Nothing
                , Empty (Vec2D 0 2), Tail (Vec2D 1 2) 4 Nothing, Head (Vec2D 2 2) (Just (Vec2D 1 2))]
      in nextFrame seed map `shouldBe` exp

    it "ages Tail tiles by one increment" $
      let map = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
                , Empty (Vec2D 0 1), Tail (Vec2D 1 1) 2 (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 1 Nothing
                , Empty (Vec2D 0 2), Tail (Vec2D 1 2) 3 (Just (Vec2D 1 1)), Empty (Vec2D 2 2) ]
          exp = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
                , Empty (Vec2D 0 1), Tail (Vec2D 1 1) 1 (Just (Vec2D 2 1)), Empty (Vec2D 2 1)
                , Empty (Vec2D 0 2), Tail (Vec2D 1 2) 2 (Just (Vec2D 1 1)), Empty (Vec2D 2 2) ]
      in nextFrame seed map `shouldBe` exp

    it "does not move Head tiles into other lines" $
      let map = [ Tail (Vec2D 0 0) 3 (Just (Vec2D 1 0)), Tail (Vec2D 1 0) 2 Nothing, Empty (Vec2D 2 0)
                , Tail (Vec2D 0 1) 4 (Just (Vec2D 0 0)), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 2 Nothing
                , Tail (Vec2D 0 2) 5 (Just (Vec2D 0 1)), Head (Vec2D 1 2) (Just (Vec2D 0 2)), Empty (Vec2D 2 2) ]
          exp = [ Tail (Vec2D 0 0) 2 (Just (Vec2D 1 0)), Tail (Vec2D 1 0) 1 Nothing, Empty (Vec2D 2 0)
                , Tail (Vec2D 0 1) 3 (Just (Vec2D 0 0)), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 1 Nothing
                , Tail (Vec2D 0 2) 4 (Just (Vec2D 0 1)), Tail (Vec2D 1 2) 4 (Just (Vec2D 0 2)), Head (Vec2D 2 2) (Just (Vec2D 1 2)) ]
      in nextFrame seed map `shouldBe` exp

  describe "runFramesR" $
    -- Not really sure how to test this one so just going to ensure it generates
    -- the right number of frames and that they are all the right size
    it "generates the right number of frames which are the same size as the original map" $ do
      let map = [ Empty (Vec2D 0 0), Empty (Vec2D 1 0), Empty (Vec2D 2 0)
                , Empty (Vec2D 0 1), Head (Vec2D 1 1) (Just (Vec2D 2 1)), Tail (Vec2D 2 1) 4 Nothing
                , Empty (Vec2D 0 2), Head (Vec2D 1 2) Nothing, Empty (Vec2D 2 2) ]
      res <- runFramesR 6 map
      length res `shouldBe` 6
      all (\x -> length x == length map) res `shouldBe` True
