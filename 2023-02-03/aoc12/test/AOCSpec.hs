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
spec = do
    describe "distance" $ do
        it "between 2 positions" $ do
            distance (0, 0, 'a') (1, 0, 'a') `shouldBe` 1
            distance (0, 0, 'a') (2, 0, 'a') `shouldBe` 2
            distance (1, 0, 'a') (4, 0, 'a') `shouldBe` 3
            distance (5, 0, 'a') (1, 0, 'a') `shouldBe` 4
            distance (3, 0, 'a') (1, 3, 'a') `shouldBe` 5
            distance (3, 1, 'a') (1, 5, 'a') `shouldBe` 6
            distance (3, 1, 'a') (1, 5, 'b') `shouldBe` 7
            distance (3, 1, 'S') (1, 5, 'c') `shouldBe` 8
            distance (3, 1, 'c') (1, 5, 'S') `shouldBe` 8
            distance (3, 1, 'y') (1, 7, 'E') `shouldBe` 9
            distance (3, 1, 'E') (1, 7, 'y') `shouldBe` 9
