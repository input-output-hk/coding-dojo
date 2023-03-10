{-# LANGUAGE ScopedTypeVariables #-}

module HelloSpec where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Hello (greetings)
import Test.Hspec (Spec, it, shouldBe, shouldReturn)

-- data RingBuffer e
--
-- -- create a new ringbuffer with the given capacity
data RingBuffer e
    = RingBuffer { capacity :: Int, buffer :: IORef (Maybe e) }

newBuffer :: Int -> IO (RingBuffer e)
newBuffer capacity = do
    x <- newIORef Nothing
    pure (RingBuffer capacity x)

--
-- -- get the capacity of the ringbuffer
-- capacity :: RingBuffer e -> IO Int
--
-- -- get the amount of free space in the ringbuffer
-- freeSpace :: RingBuffer e -> IO Int
--
-- -- returns true if it could push, false if it couldn't (no capacity)
push :: e -> RingBuffer e -> IO Bool

push x (RingBuffer capacity ioref) 
  | capacity == 0 = return False 
  | otherwise = do
      writeIORef ioref (Just x)
      pure True

--
-- -- pop an element off the ringbuffer, returns Nothing if there isn't one
pop :: RingBuffer e -> IO (Maybe e)
pop (RingBuffer _ ioref) = do 
  v <- readIORef ioref
  writeIORef ioref Nothing
  return v

spec :: Spec
spec = do
    it "pop on new buffer yields Nothing" $ do
        (buffer :: RingBuffer Int) <- newBuffer 42
        pop buffer `shouldReturn` Nothing

    it "push and pop the same thing should come back" $ do
        (buffer :: RingBuffer Int) <- newBuffer 42
        push 79 buffer `shouldReturn` True
        pop buffer `shouldReturn` Just 79

    it "a thing should be gone after it is popped" $ do
        (buffer :: RingBuffer Int) <- newBuffer 42
        push 79 buffer 
        pop buffer
        pop buffer `shouldReturn` Nothing

    it "push over capacity" $ do
        (buffer :: RingBuffer Int) <- newBuffer 0
        push 79 buffer `shouldReturn` False


-- Run me with `cabal test --test-show-details=direct`
-- 
