-- | Day 11: RTG
module Day11 where

data Type = Chip | Generator
type Element = String
data Item = (Element, Type)

type Floor = [Item]
type Building = (Floor, Floor, Floor, Floor)
type Elevator = a | b | c | d
type State = (Building, Elevator)

solve11 :: Int
solve11 = 42
