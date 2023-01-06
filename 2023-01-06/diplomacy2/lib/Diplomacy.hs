module Diplomacy where

data Province = Venice | Rome
         deriving (Eq, Show)

data Unit = Army Province
   deriving (Eq, Show)

data Country = Austria
   deriving (Eq, Show)

data Order = Move Unit Province

type Position = (Country, Unit)
type GameMap = [(Province, Province)]

solve :: [(Country, Order)] -> [Position] -> GameMap -> [Position]

solve [] oob _ = oob

solve _ [ (Austria, Army Venice) ] [ (Venice, Rome) ] = [(Austria, Army Rome)]
