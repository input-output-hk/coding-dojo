module RingBuffer where

import Control.Monad (forM, liftM, replicateM)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (tails)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..))
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as Vector
import Debug.Trace (trace)
import GHC.Clock (getMonotonicTime)
import GHC.Natural (Natural)

pop :: RingBuffer -> IO (Maybe Int)
pop (RingBuffer capacity index ref) = do
    toPop <- atomicModifyIORef index $ \(first, last) ->
        trace ("first=" <> show first <> ", last=" <> show last) $
            if isEmpty capacity first last
                then ((first, last), Nothing)
                else ((first, succ last), Just last)
    case toPop of
        Nothing -> pure Nothing
        Just ix -> Just <$> Vector.read ref (ix `mod` capacity)

push :: RingBuffer -> Int -> IO Bool
push (RingBuffer capacity index ref) x = do
    (first, last) <- readIORef index
    if isFull capacity first last
        then pure False
        else trace ("first=" <> show first <> ", last=" <> show last) $ do
            writeIORef index (succ first, last)
            Vector.write ref (first `mod` capacity) x
            pure True

isFull :: Int -> Int -> Int -> Bool
isFull capacity first last =
  first - last == capacity

isEmpty :: Int -> Int -> Int -> Bool
isEmpty capacity first last = first == last

data RingBuffer = RingBuffer
    { -- |Capacity of the buffer
      capacity :: !Int
    , -- |Mutable indices for next available index and next index to pop
      indices :: !(IORef (Int, Int))
    , -- |Mutable vector of size `capacity`
      vector :: !(IOVector Int)
    }

newBuffer :: Natural -> IO RingBuffer
newBuffer capacity = do
    let cap = fromIntegral capacity
    RingBuffer cap <$> newIORef (0, 0) <*> Vector.new cap
