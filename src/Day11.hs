-- | Day 11: RTG
module Day11 where
import Prelude hiding (floor)
import Data.List.Split
import Data.List
import Data.Maybe

data Type = Chip | Generator deriving ( Show, Eq )
type Element = String
type Item = (Element, Type)
type Cargo = [Item]

data Level = First | Second | Third | Fourth deriving ( Show, Enum, Eq )
type Floor = (Level, Cargo)
type Building = [Floor]
type Direction = Level -> Level
down :: Direction
down = pred
up :: Direction
up = succ
-- type Direction = Level -> Maybe Level
-- down :: Direction
-- down First = Nothing
-- down x = Just $ pred x
-- up :: Direction
-- up Fourth = Nothing
-- up x = Just $ succ x

type State = (Building, Level)

example :: IO String
example = readFile "data/example11.txt"

-- | parse input
parse :: String -> Building
parse = zip [First .. Fourth] . map (foldr (filterItems . toItem ) [] .  splitOn "a ") . lines
    where
        filterItems :: Maybe Item -> [Item] -> [Item]
        filterItems (Just x) xs = x:xs
        filterItems Nothing xs = xs

-- | parse substring of description
toItem :: String -> Maybe Item
toItem s
    | "microchip" `isInfixOf` s = Just (takeWhile (/= '-') s, Chip)
    | "generator" `isInfixOf` s = Just (takeWhile (/= ' ') s, Generator)
    | otherwise = Nothing

-- | if there is a generator on the floo
-- every chip must be powered
floorSafe :: Floor -> Bool
floorSafe (_, xs)
    | Generator `notElem` map snd xs = True
    | otherwise = let chips = filter ((Chip ==) . snd) xs
                      gens = filter ((Generator ==) . snd) xs
                      isPowered :: Item -> Bool
                      isPowered c = fst c `elem` map fst gens
                  in all isPowered chips

buildingSafe :: Building -> Bool
buildingSafe = all floorSafe

stateSafe :: State -> Bool
stateSafe = all floorSafe . fst

-- | Starting state from text example
exampleState :: State
exampleState = ([(First, [("hydrogen",Chip),("lithium",Chip)]),
                 (Second, [("hydrogen",Generator)]),
                 (Third, [("lithium",Generator)]),
                 (Fourth, [])
                 ], First)

-- | move elevator with cargo 
-- >>> move [("hydrogen", Chip)] up exampleState
-- ([(First, [("lithium",Chip)]), (Second, [("hydrogen",Chip),("hydrogen",Generator)]), (Third, [("lithium",Generator)]), (Fourth, [])], Second)
move :: Cargo -> Direction -> State -> State
move cargo dir state = (building', elevator')
    where 
          elevator = snd state
          elevator' = dir elevator
          building = fst state
          -- floor = fst state !! fromEnum elevator
          building' = updateBuilding cargo elevator elevator' building

          updateBuilding :: Cargo -> Level -> Level -> Building -> Building
          updateBuilding items from to = map (updateFloor items from to)

          updateFloor :: Cargo -> Level -> Level -> Floor -> Floor
          updateFloor cargo' from to (level, items)
              | level == from = (level, filter (`notElem` cargo') items)
              | level == to = (level, items ++ cargo')
              | otherwise = (level, items)

next :: State -> [State]
next state@(building, elevator@First) = filter stateSafe $
    [move cargo up state | cargo <- [[floorItems!!i, floorItems!!j] | i <- [0..n-1], j <- [0..n-1], i < j]]
    ++ [move cargo up state | cargo <- map return floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
          n = length floorItems
next state@(building, elevator@Fourth) = filter stateSafe $
    [move cargo down state | cargo <- [[floorItems!!i, floorItems!!j] | i <- [0..n-1], j <- [0..n-1], i < j]]
    ++ [move cargo down state | cargo <- map return floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
          n = length floorItems
next state@(building, elevator) = filter stateSafe $
    [move cargo dir state | dir <- [up, down], cargo <- [[floorItems!!i, floorItems!!j] | i <- [0..n-1], j <- [0..n-1], i < j]]
    ++ [move cargo down state | cargo <- map return floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
          n = length floorItems

solved :: State -> Bool
solved (building, _) = n == length (building !! 3)    
    where n = sum $ map (length . snd) building

-- | init >>= next >>= next ....
solve11 :: [State]
solve11 = next exampleState >>= next >>= next
-- solve11 = next exampleState >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next
