module AOCSpec where

import AOC (distance)
import Test.Hspec (Spec, describe, it, pending, shouldBe)

-- Sabqponm
-- abcryxxl
-- accszExk
-- acctuvwj
-- abdefghi
--
-- result = 31 steps

-- algo:
--  looks like a A*?
--  shortest path in a graph -> needs backtracking + evaluation
--  need to compute distance to goal -> in 3D -> manhattan distance
--  evaluate quality of move?
--
-- 1. how to representin the input ?
-- 2. compute one move
--    take a position and a move -> compute positiong and a move

spec :: Spec
spec = describe "distance" $ do
    it "between 2 positions" $ do
        distance (0, 0, 'a') (1, 0, 'a') `shouldBe` 1
        distance (0, 0, 'a') (2, 0, 'a') `shouldBe` 2
        distance (1, 0, 'a') (4, 0, 'a') `shouldBe` 3
        distance (5, 0, 'a') (1, 0, 'a') `shouldBe` 4
