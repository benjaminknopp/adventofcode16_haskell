-- | Day 11: RTG
module Day11 where
import Data.List.Split
import Data.List

data Type = Chip | Generator deriving ( Show, Eq )
type Element = String
type Item = (Element, Type)
type Cargo = [Item]

type Floor = [Item]
-- type Building = (Floor, Floor, Floor, Floor)
type Building = [Floor]
data Elevator = First | Second | Third | Fourth deriving ( Show, Enum )
type Direction = Elevator -> Elevator
down :: Direction
down = pred
up :: Direction
up = succ

type State = (Building, Elevator)

example :: IO String
example = readFile "data/example11.txt"

-- | parse input
parse :: String -> Building
parse = map (foldr (_filterItems . toItem ) [] .  splitOn "a ") . lines
    where
        _filterItems :: Maybe Item -> [Item] -> [Item]
        _filterItems (Just x) xs = x:xs
        _filterItems Nothing xs = xs

-- | parse substring of description
toItem :: String -> Maybe Item
toItem s
    | "microchip" `isInfixOf` s = Just (takeWhile (/= '-') s, Chip)
    | "generator" `isInfixOf` s = Just (takeWhile (/= ' ') s, Generator)
    | otherwise = Nothing

-- | if there is a generator on the floo
-- every chip must be powered
isSafe :: Floor -> Bool
isSafe xs
    | Generator `notElem` map snd xs = True
    | otherwise = let chips = filter ((Chip ==) . snd) xs
                      gens = filter ((Generator ==) . snd) xs
                      isPowered :: Item -> Bool
                      isPowered c = fst c `elem` map fst gens
                  in all isPowered chips

exampleState :: State
exampleState = ([[("hydrogen",Chip),("lithium",Chip)],[("hydrogen",Generator)],[("lithium",Generator)],[]], First)
-- | move elevator with cargo >>> move [("hydrogen", Chip)] up exampleState ([[("lithium",Chip)],[("hydrogen",Chip),("hydrogen",Generator)],[("lithium",Generator)],[]], First)
move :: Cargo -> Direction -> State -> Maybe State
move cargo dir state
    | (not . all (`elem` floor)) cargo = Nothing
    | otherwise = Just ([floor, floor', floor, floor], elevator')
    where 
          elevator = snd state
          elevator' = dir elevator
          floor = (fst state) !! fromEnum elevator
          floor' = fst state !! fromEnum elevator'

-- :: Cargo -> Elevator -> Elevator -> Building -> Building
-- items from to building

_mg :: Eq a => ([a], [a]) -> a -> Maybe ([a], [a])
_mg (from, to) item
    | item `notElem` from = Nothing
    | otherwise = Just (filter (/= item) from, item:to)

solve11 :: Int
solve11 = 42
