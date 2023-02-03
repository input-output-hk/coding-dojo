module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance (x, y, _) (x', y', _) = abs (x' - x) + abs (y - y')
