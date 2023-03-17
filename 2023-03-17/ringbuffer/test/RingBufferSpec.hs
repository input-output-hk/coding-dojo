module RingBufferSpec where

import Control.Monad (liftM, replicateM)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import GHC.Natural
import Test.Hspec (Spec, it, pending, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..), Property, arbitrary, counterexample, forAll)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

-- a "ring" buffer where you can:
-- - push an element to the tail if not full
-- - pop an element from the head if not empty
-- - fixed capacity in the queue

spec :: Spec
spec =
    prop "pushing an element then popping it gives back same element" pushPopIsIdempotence

pushPopIsIdempotence :: Positive Integer -> Property
pushPopIsIdempotence (Positive capacity) = forAll arbitrary $ \xs -> monadicIO $ do
    ys <- run $ do
        buffer <- newBuffer (fromInteger capacity)
        traverse_ (push buffer) xs
        replicateM (length xs) (pop buffer)

    monitor $ counterexample $ "popped from buffer: " <> show ys
    assert (xs == ys)

pop :: RingBuffer -> IO Int
pop (RingBuffer _ ref) = do
    atomicModifyIORef ref (\(x : xs) -> (xs, x))

push :: RingBuffer -> Int -> IO ()
push (RingBuffer _ xs) x = modifyIORef xs (<> [x])

data RingBuffer = RingBuffer Natural (IORef [Int])

newBuffer :: Natural -> IO RingBuffer
newBuffer capacity = liftM (RingBuffer capacity) (newIORef [])
