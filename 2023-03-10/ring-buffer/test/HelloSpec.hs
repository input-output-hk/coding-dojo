module HelloSpec where

import Test.Hspec(Spec, shouldBe)
import Hello(greetings)


-- data RingBuffer e
-- 
-- -- create a new ringbuffer with the given capacity
-- newBuffer :: Int -> IO (RingBuffer e)
-- 
-- -- get the capacity of the ringbuffer
-- capacity :: RingBuffer e -> IO Int
-- 
-- -- get the amount of free space in the ringbuffer
-- freeSpace :: RingBuffer e -> IO Int
-- 
-- -- returns true if it could push, false if it couldn't (no capacity)
-- push :: e -> RingBuffer e -> IO Bool
-- 
-- -- pop an element off the ringbuffer, returns Nothing if there isn't one
-- pop :: RingBuffer e -> IO (Maybe e)

spec :: Spec
spec = do
  it "pop on new buffer yields Nothing" $ do
    buffer <- newBuffer 42
    pop buffer `shouldReturn` Nothing 
    

-- Run me with `cabal test --test-show-details=direct`
