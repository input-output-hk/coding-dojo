{-# LANGUAGE LambdaCase #-}

module RingBufferSpec where

import Control.Monad (forM, liftM, replicateM)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (tails)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..))
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import GHC.Clock (getMonotonicTime)
import GHC.Natural (Natural)
import RingBuffer
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
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
            \(Positive capacity) ->
                forAll arbitrary $ \(NonNegative first) ->
                    forAll arbitrary $ \(NonNegative last) ->
                        last < first
                            ==> isFull capacity first last == (first - last == capacity)
    describe "isEmpty" $ do
        prop "should be true given last equals first" $
            \(Positive capacity) (NonNegative idx) ->
                isEmpty capacity idx idx

    prop "pushing an element then popping it gives back same element" pushPopIsIdempotence
    prop "pushing and popping an element is in O(1)" bufferAccessTimeIsConstant

bufferAccessTimeIsConstant :: Property
bufferAccessTimeIsConstant =
    forAll (resize 100 arbitrary) $ \xs -> monadicIO $ do
        let subs = filter (not . null) $ tails xs
        ys <- run $
            forM subs $ \sublist -> do
                buffer <- newBuffer (fromIntegral $ length sublist)
                st <- getMonotonicTime
                traverse_ (push buffer) sublist
                replicateM (fromIntegral $ length sublist) (pop buffer)
                end <- getMonotonicTime
                pure ((end - st) / fromIntegral (length sublist))

        monitor $ counterexample $ "popping time: " <> show (map (* 1000000) ys)
        assert (all (uncurry (==)) $ zip ys (tail ys))

pushPopIsIdempotence :: Property
pushPopIsIdempotence =
    forAll arbitrary $ \xs ->
        forAll arbitrary $ \(Positive capacity) ->
            capacity <= length xs ==> monadicIO $ do
                ys <- run $ do
                    buffer <- newBuffer (fromIntegral capacity)
                    traverse_ (push buffer) xs
                    replicateM (fromIntegral capacity) (pop buffer)

                monitor $ counterexample $ "popped: " <> show ys
                assert (take capacity xs == catMaybes ys)
