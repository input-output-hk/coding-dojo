{-# LANGUAGE ScopedTypeVariables #-}

module HelloSpec where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Hello (greetings)
import Test.Hspec (Spec, it, shouldBe, shouldReturn)

-- data RingBuffer e
--
-- -- create a new ringbuffer with the given capacity
data RingBuffer e
    = RingBuffer (IORef (Maybe e))

newBuffer :: Int -> IO (RingBuffer e)
newBuffer _ = do
    x <- newIORef Nothing
    pure (RingBuffer x)

--
-- -- get the capacity of the ringbuffer
-- capacity :: RingBuffer e -> IO Int
--
-- -- get the amount of free space in the ringbuffer
-- freeSpace :: RingBuffer e -> IO Int
--
-- -- returns true if it could push, false if it couldn't (no capacity)
push :: e -> RingBuffer e -> IO Bool
push x (RingBuffer ioref) = do
  writeIORef ioref (Just x)
  pure True

--
-- -- pop an element off the ringbuffer, returns Nothing if there isn't one
pop :: RingBuffer e -> IO (Maybe e)
pop (RingBuffer ioref) = readIORef ioref

spec :: Spec
spec = do
    it "pop on new buffer yields Nothing" $ do
        (buffer :: RingBuffer Int) <- newBuffer 42
        pop buffer `shouldReturn` Nothing

    it "push and pop the same thing should come back" $ do
        (buffer :: RingBuffer Int) <- newBuffer 42
        push 79 buffer `shouldReturn` True
        pop buffer `shouldReturn` Just 79

-- Run me with `cabal test --test-show-details=direct`
