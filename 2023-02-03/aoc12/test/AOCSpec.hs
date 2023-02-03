module AOCSpec where

import Test.Hspec (Spec, it, pending, shouldBe)

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
spec = it "does something" $ 1 `shouldBe` 1
