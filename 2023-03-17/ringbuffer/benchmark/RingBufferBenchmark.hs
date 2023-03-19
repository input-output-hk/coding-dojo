{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Monad (replicateM)
import Criterion (bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.Foldable (traverse_)
import RingBuffer (newBuffer, pop, push)
import System.Random ()
import System.Random.Stateful (randomRIO)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    [l1000, l10_000, l100_000] <-
        mapM
            randomList
            [1000, 10_000, 100_000]
    defaultMain
        [ bgroup
            "Ring Buffer tests"
            [ bench "Push/Pop 1000" $ whnfIO $ pushAndPop l1000
            , bench "Push/Pop 10000" $ whnfIO $ pushAndPop l10_000
            , bench "Push/Pop 100000" $ whnfIO $ pushAndPop l100_000
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
