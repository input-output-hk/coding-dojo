module Diplomacy where

import qualified Data.List as List

data Province = Venice | Rome
    deriving (Eq, Show, Ord, Enum, Bounded)

data Unit = Army Province
    deriving (Eq, Show)

data Country = Austria
    deriving (Eq, Show, Ord, Enum, Bounded)

data Order = Move Unit Province
    deriving (Eq, Show)

type Position = (Country, Unit)
type GameMap = [(Province, Province)]

solve :: [(Country, Order)] -> [Position] -> GameMap -> [Position]
solve [] oob _ = oob
solve ((cty, Move (Army src) dest) : orders) positions map =
    let positions' = List.delete (cty, Army src) positions
     in (cty, Army dest) : solve orders positions' map
