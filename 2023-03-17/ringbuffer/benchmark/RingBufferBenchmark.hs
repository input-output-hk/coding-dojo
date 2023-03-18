module Main where

import Control.Monad (replicateM, replicateM_)
import Criterion (bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.Foldable (traverse_)
import RingBuffer (RingBuffer (capacity), newBuffer, pop, push)
import System.Random ()
import System.Random.Stateful (randomRIO)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    [l1, l2, l3, l4, l5, l6] <-
        mapM
            randomList
            [1, 10, 100, 1000, 10000, 100000]
    defaultMain
        [ bgroup
            "fences tests"
            [ bench "Size 1000 Test" $ whnfIO $ pushAndPop l4
            , bench "Size 10000 Test" $ whnfIO $ pushAndPop l5
            , bench "Size 100000 Test" $ whnfIO $ pushAndPop l6
            ]
        ]

pushAndPop :: [Int] -> IO [Int]
pushAndPop xs = catMaybes <$> do
    let capacity = length xs
    buffer <- newBuffer (fromIntegral capacity)
    traverse_ (push buffer) xs
    replicateM (fromIntegral capacity) (pop buffer)

randomList :: Int -> IO [Int]
randomList n = replicateM n (randomRIO (1, 10000 :: Int))
