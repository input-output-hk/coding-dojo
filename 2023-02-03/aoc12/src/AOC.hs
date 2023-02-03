module AOC where

type Cell = (Int, Int, Char)

distance :: Cell -> Cell -> Int
distance (x, y, c) (x', y', c') =
    let zIndex char = case char of
            'S' -> 'a'
            'E' -> 'z'
            _ -> char
        z = zIndex c
        z' = zIndex c'
     in abs (x' - x) + abs (y - y') + abs (fromEnum z - fromEnum z')

type Position = (Int, Int)
