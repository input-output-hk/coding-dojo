* Ring buffer

```
data RingBuffer e

-- create a new ringbuffer with the given capacity
newBuffer :: Int -> IO (RingBuffer e)

-- get the capacity of the ringbuffer
capacity :: RingBuffer e -> IO Int

-- get the amount of free space in the ringbuffer
freeSpace :: RingBuffer e -> IO Int

-- returns true if it could push, false if it couldn't (no capacity)
push :: e -> RingBuffer e -> IO Bool

-- pop an element off the ringbuffer, returns Nothing if there isn't one
pop :: RingBuffer e -> IO (Maybe e)
```
