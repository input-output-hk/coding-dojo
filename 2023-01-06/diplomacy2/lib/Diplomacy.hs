module Diplomacy where

import qualified Data.List as List

data Province = Venice | Rome | Tuscany | Piemont | Puglia | Naples
    deriving (Eq, Show, Ord, Enum, Bounded)

allProvinces :: [Province]
allProvinces = enumFromTo minBound maxBound

data Unit = Army Province
    deriving (Eq, Show)

data Country = Austria
    deriving (Eq, Show, Ord, Enum, Bounded)

allCountries :: [Country]
allCountries = enumFromTo minBound maxBound

data Order = Move Country Unit Province
    deriving (Eq, Show)

type Position = (Country, Unit)
type GameMap = [(Province, Province)]

adjacent :: GameMap -> Province -> Province -> Bool
adjacent map p1 p2 = (p1, p2) `elem` map || (p2, p1) `elem` map

neighbours :: Province -> GameMap -> [Province]
neighbours province = foldr (neighbour province) []

neighbour :: Province -> (Province, Province) -> [Province] -> [Province]
neighbour p (p1, p2) ps
    | p == p1 && p2 `notElem` ps = p2 : ps
    | p == p2 && p1 `notElem` ps = p1 : ps
    | otherwise = ps

defaultMap :: GameMap
defaultMap =
    [ (Venice, Rome)
    , (Venice, Tuscany)
    , (Venice, Piemont)
    , (Rome, Tuscany)
    , (Rome, Naples)
    , (Rome, Puglia)
    , (Naples, Puglia)
    ]

solve :: [Order] -> [Position] -> GameMap -> [Position]
solve [] oob _ = oob
solve (Move cty (Army src) dest : orders) positions map =
    let positions' = List.delete (cty, Army src) positions
     in (cty, Army dest) : solve orders positions' map
