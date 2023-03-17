{-# LANGUAGE LambdaCase #-}

module RingBufferSpec where

import Control.Monad (liftM, replicateM)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import GHC.Natural (Natural)
import Test.Hspec (Spec, it, pending, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..), Property, arbitrary, counterexample, forAll, (==>))
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
    prop "pushing an element then popping it gives back same element" pushPopIsIdempotence

pushPopIsIdempotence :: Property
pushPopIsIdempotence =
    forAll arbitrary $ \xs ->
        forAll arbitrary $ \(Positive capacity) ->
            capacity <= length xs ==> monadicIO $ do
                ys <- run $ do
                    buffer <- newBuffer (fromIntegral capacity)
                    traverse_ (push buffer) xs
                    replicateM (fromIntegral capacity) (pop buffer)

                monitor $ counterexample $ "popped from buffer: " <> show ys
                assert (take capacity xs == catMaybes ys)

pop :: RingBuffer -> IO (Maybe Int)
pop (RingBuffer _ ref) = do
    atomicModifyIORef ref (\case (x : xs) -> (xs, Just x); [] -> ([], Nothing))

push :: RingBuffer -> Int -> IO Bool
push (RingBuffer capacity ref) x =
    atomicModifyIORef ref $ \xs ->
        if length xs < fromIntegral capacity
            then (xs <> [x], True)
            else (xs, False)

data RingBuffer = RingBuffer Natural (IORef [Int])

newBuffer :: Natural -> IO RingBuffer
newBuffer capacity = liftM (RingBuffer capacity) (newIORef [])
