module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance (x, _, _) (x', y', _) = abs (x' - x) + y'
