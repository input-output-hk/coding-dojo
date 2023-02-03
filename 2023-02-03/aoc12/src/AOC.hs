module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance (x, y, z) (x', y', z') = abs (x' - x) + abs (y - y') + abs (fromEnum z - fromEnum z')
