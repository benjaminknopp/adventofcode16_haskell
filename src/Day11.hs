-- | Day 11: RTG
module Day11 where
import Prelude hiding (floor)
import Data.List.Split
import Data.List

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
isSafe :: Floor -> Bool
isSafe (_, xs)
    | Generator `notElem` map snd xs = True
    | otherwise = let chips = filter ((Chip ==) . snd) xs
                      gens = filter ((Generator ==) . snd) xs
                      isPowered :: Item -> Bool
                      isPowered c = fst c `elem` map fst gens
                  in all isPowered chips

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
move :: Cargo -> Direction -> State -> Maybe State
move cargo dir state
    | (not . all (`elem` snd floor)) cargo = Nothing
    | otherwise = Just (building', elevator')
    where 
          elevator = snd state
          elevator' = dir elevator
          building = fst state
          floor = fst state !! fromEnum elevator
          building' = updateBuilding cargo elevator elevator' building

updateBuilding :: Cargo -> Level -> Level -> Building -> Building
updateBuilding items from to = map (updateFloor items from to)

updateFloor :: Cargo -> Level -> Level -> Floor -> Floor
updateFloor cargo from to (level, items)
    | level == from = (level, filter (`notElem` cargo) items)
    | level == to = (level, items ++ cargo)
    | otherwise = (level, items)

solve11 :: Int
solve11 = 42
