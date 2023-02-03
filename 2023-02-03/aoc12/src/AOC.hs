module AOC where

import Control.Monad (guard)

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

type Grid = [String]

allPossibleMoves :: Position -> [Position]
allPossibleMoves (x, y) = do
    x' <- [x - 1, x, x + 1]
    y' <- [y - 1, y, y + 1]
    guard $ y' >= 0
    guard $ x' >= 0
    guard $ x' == x || y' == y
    guard $ x' /= x || y' /= y
    pure (x', y')
