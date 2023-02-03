module AOC where

type Position = (Int, Int, Char)

distance :: Position -> Position -> Int
distance (x, y, c) (x', y', c') =
    let z = case c of
            'S' -> 'a'
            'E' -> 'z'
            _ -> c
        z' = case c' of
            'S' -> 'a'
            'E' -> 'z'
            _ -> c'
     in abs (x' - x) + abs (y - y') + abs (fromEnum z - fromEnum z')
