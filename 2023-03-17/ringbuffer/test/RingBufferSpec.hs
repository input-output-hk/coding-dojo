{-# LANGUAGE LambdaCase #-}

module RingBufferSpec where

import Control.Monad (forM, replicateM, replicateM_)
import Data.Foldable (traverse_)
import Data.List (tails)
import Data.Maybe (catMaybes)
import GHC.Clock (getMonotonicTime)
import RingBuffer
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop, xprop)
import Test.QuickCheck (NonNegative (..), Positive (..), Property, arbitrary, counterexample, forAll, resize, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

-- a "ring" buffer where you can:
-- - push an element to the tail if not full
-- - pop an element from the head if not empty
-- - fixed capacity in the queue

spec :: Spec
spec = do
    it "return nothing for pop of empty buffer" $ do
        b <- newBuffer 0
        pop b `shouldReturn` Nothing
    it "return False when pushing on full buffer" $ do
        b <- newBuffer 0
        push b 42 `shouldReturn` False

    describe "isFull" $ do
        it "should be true given capacity, first and last is 0" $ isFull 0 0 0 `shouldBe` True
        prop "should be false given capacity is non 0, last is lower than first, and first minus last is less than capacity" $
            \(Positive size) ->
                forAll arbitrary $ \(NonNegative pushIndex) ->
                    forAll arbitrary $ \(NonNegative popIndex) ->
                        popIndex < pushIndex
                            ==> isFull size pushIndex popIndex == (pushIndex - popIndex == size)
    describe "isEmpty" $ do
        prop "should be true given last equals first" $
            \(NonNegative idx) ->
                isEmpty idx idx

    prop "pushing an element then popping it gives back same element" pushPopIsIdempotence
    xprop "pushing and popping an element is in O(1)" bufferAccessTimeIsConstant

bufferAccessTimeIsConstant :: Property
bufferAccessTimeIsConstant =
    forAll (resize 100 arbitrary) $ \xs -> monadicIO $ do
        let subs = filter (not . null) $ tails xs
        ys <- run $
            forM subs $ \sublist -> do
                buffer <- newBuffer (fromIntegral $ length sublist)
                st <- getMonotonicTime
                traverse_ (push buffer) sublist
                replicateM_ (fromIntegral $ length sublist) (pop buffer)
                end <- getMonotonicTime
                pure ((end - st) / fromIntegral (length sublist))

        monitor $ counterexample $ "popping time: " <> show (map (* 1000000) ys)
        assert (all (uncurry (==)) $ zip ys (tail ys))

pushPopIsIdempotence :: Property
pushPopIsIdempotence =
    forAll arbitrary $ \xs ->
        forAll arbitrary $ \(Positive size) ->
            size <= length xs ==> monadicIO $ do
                ys <- run $ do
                    buffer <- newBuffer (fromIntegral size)
                    traverse_ (push buffer) xs
                    replicateM (fromIntegral size) (pop buffer)

                monitor $ counterexample $ "popped: " <> show ys
                assert (take size xs == catMaybes ys)
