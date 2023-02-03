module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance (x, y, c) (x', y', c') =
    let zIndex char = case char of
            'S' -> 'a'
            'E' -> 'z'
            _ -> char
        z = zIndex c
        z' = zIndex c'
     in abs (x' - x) + abs (y - y') + abs (fromEnum z - fromEnum z')
