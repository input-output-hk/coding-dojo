module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance _ (x, _, _) = x
