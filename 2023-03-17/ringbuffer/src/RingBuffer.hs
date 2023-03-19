{-# LANGUAGE NamedFieldPuns #-}
module RingBuffer where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Data.Word (Word64)

data RingBuffer = RingBuffer
    { -- |Capacity of the buffer
      capacity :: !Word64
    , -- |Mutable indices for next available index and next index to pop
      indices :: !(IORef (Word64, Word64))
    , -- |Mutable vector of size `capacity`
      vector :: !(IOVector Int)
    }

newBuffer :: Word64 -> IO RingBuffer
newBuffer size = do
    RingBuffer size <$> newIORef (0, 0) <*> Vector.new (fromIntegral size)

isFull :: Word64 -> Word64 -> Word64 -> Bool
isFull size pushIndex popIndex =
    pushIndex - popIndex == size

isEmpty :: Word64 -> Word64 -> Bool
isEmpty pushIndex popIndex = pushIndex == popIndex

pop :: RingBuffer -> IO (Maybe Int)
pop RingBuffer{ capacity, indices, vector} = do
    toPop <- atomicModifyIORef' indices $ \(pushIndex, popIndex) ->
        if isEmpty pushIndex popIndex
            then ((pushIndex, popIndex), Nothing)
            else ((pushIndex, succ popIndex), Just popIndex)
    case toPop of
        Nothing -> pure Nothing
        Just ix -> Just <$> Vector.read vector (fromIntegral $ ix `mod` capacity)

push :: RingBuffer -> Int -> IO Bool
push RingBuffer{ capacity, indices, vector} x = do
    (pushIndex, popIndex) <- readIORef indices
    if isFull capacity pushIndex popIndex
        then pure False
        else do
            writeIORef indices (succ pushIndex, popIndex)
            Vector.write vector (fromIntegral $ pushIndex `mod` capacity) x
            pure True
