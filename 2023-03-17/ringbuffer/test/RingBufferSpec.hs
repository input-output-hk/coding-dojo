module RingBufferSpec where

import Control.Monad (liftM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Test.Hspec (Spec, it, pending, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, forAll)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- a "ring" buffer where you can:
-- - push an element to the tail if not full
-- - pop an element from the head if not empty
-- - fixed capacity in the queue

spec :: Spec
spec =
    prop "pushing an element then popping it gives back same element" pushPopIsIdempotence

pushPopIsIdempotence :: Property
pushPopIsIdempotence = forAll arbitrary $ \x -> monadicIO $ do
    y <- run $ do
        buffer <- newBuffer
        push buffer x
        pop buffer

    assert (x == y)

pop :: RingBuffer -> IO Int
pop (RingBuffer b) = readIORef b

push :: RingBuffer -> Int -> IO ()
push (RingBuffer b) x = writeIORef b x

data RingBuffer = RingBuffer (IORef Int)

newBuffer :: IO RingBuffer
newBuffer = liftM RingBuffer (newIORef 0)
