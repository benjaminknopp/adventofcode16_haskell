{-# LANGUAGE TupleSections #-}

-- | Day 11: RTG
module Day11 where
import Prelude hiding (floor)
import Data.List.Split
import Data.List
-- import Data.Set
import Control.Monad

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

-- | each state is characterized by the building configuration and elevator level
type State = (Building, Level)

-- | parse input
-- example:
-- The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
-- The second floor contains a hydrogen generator.
-- The third floor contains a lithium generator.
-- The fourth floor contains nothing relevant.
parse :: String -> Building
parse = zip [First .. Fourth] . map (foldr (filterItems . toItem ) [] .  splitOn "a ") . lines
    where
        toItem :: String -> Maybe Item
        toItem s
            | "microchip" `isInfixOf` s = Just (takeWhile (/= '-') s, Chip)
            | "generator" `isInfixOf` s = Just (takeWhile (/= ' ') s, Generator)
            | otherwise = Nothing
        filterItems :: Maybe Item -> [Item] -> [Item]
        filterItems (Just x) xs = x:xs
        filterItems Nothing xs = xs

-- | if there is a generator on the floor
-- every chip must be powered
floorSafe :: Floor -> Bool
floorSafe (_, xs)
    | Generator `notElem` map snd xs = True
    | otherwise = let chips = filter ((Chip ==) . snd) xs
                      gens = filter ((Generator ==) . snd) xs
                      isPowered :: Item -> Bool
                      isPowered c = fst c `elem` map fst gens
                  in all isPowered chips

-- | check constraints on state
stateSafe :: State -> Bool
stateSafe = all floorSafe . fst

-- | move elevator with cargo 
-- >>> move [("hydrogen", Chip)] up exampleState
-- ([(First, [("lithium",Chip)]), (Second, [("hydrogen",Chip),("hydrogen",Generator)]), (Third, [("lithium",Generator)]), (Fourth, [])], Second)
move :: Cargo -> Direction -> State -> State
move cargo dir state = (building', elevator')
    where 
          updateBuilding :: Cargo -> Level -> Level -> Building -> Building
          updateBuilding items from to = map (updateFloor items from to)

          updateFloor :: Cargo -> Level -> Level -> Floor -> Floor
          updateFloor cargo' from to (level, items)
              | level == from = (level, filter (`notElem` cargo') items)
              | level == to = (level, items ++ cargo')
              | otherwise = (level, items)

          elevator = snd state
          elevator' = dir elevator
          building = fst state
          building' = updateBuilding cargo elevator elevator' building

nextAll :: State -> [State]
nextAll state@(building, elevator@First) = _generateNextStates building elevator state up up
nextAll state@(building, elevator@Fourth) = [move cargo down state | cargo <- map return floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
nextAll state@(building, elevator) = _generateNextStates building elevator state up down
    -- -- [move cargo dir state | dir <- [up, down], cargo <- [[floorItems!!i, floorItems!!j] | i <- [0..n-1], j <- [0..n-1], i < j]]
    -- -- ++ [move cargo down state | cargo <- map return floorItems]

next :: State -> [State]
next = filter stateSafe . nextAll

next' :: [State] -> State -> ([State], [State])
next' visited state = (visited, filter stateSafe (nextAll state))

_generateNextStates :: Building -> Level -> State -> Direction -> Direction -> [State]
_generateNextStates building elevator state dir1 dir2 = 
    [move cargo dir1 state | cargo <- [[floorItems!!i, floorItems!!j] | i <- [0..n-1], j <- [0..n-1], i < j]]
    ++ [move cargo dir2 state | cargo <- map return floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
          n = length floorItems

-- | check for final state
solved :: State -> Bool
solved (building, _) = n == (length . snd) (building !! fromEnum Third)
    where n = sum $ map (length . snd) building

-- | init >>= next >>= next ....
-- solve11 :: [State]
-- solve11 = next exampleState
-- solve11 = next exampleState >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next >>= next
repeatNext :: State -> Int -> [State]
repeatNext start x = foldr (<=<) return (replicate x next) start

-- solve11' :: State -> Int -> Bool
-- solve11' start x = any solved $ repeatNext start x

diRec :: Int -> [State] -> Int
diRec n states
    | any solved states = n
    | otherwise = diRec (n+1) (states >>= next)

-- day11 :: IO ()
-- day11 = do
--     -- input <- fmap parse example
--     input <- parse <$> readFile "data/input11.txt"
--     let start = (input, First)
--     let n = diRec 0 $ return start
--     print n
--     -- let x = takeWhile (not . solve11' start) [30..]
--     -- print x

parseState :: String -> [(Building, Level)]
parseState = return . (, First) . parse

readBuilding :: String -> IO String
-- readBuilding =  readFile "data/input11.txt"
readBuilding s = readFile ("data/"++s++"11.txt")

day11 :: IO ()
day11 = do
    -- readBuilding >>= displayStates . return . (, First) . parse
    readBuilding "example" >>= (print . length) . (return <=< parseState)
    line <- getLine
    if line == "q"
        then return ()
        else do
            -- readBuilding >>= displayStates . next . (, First) . parse
            -- readBuilding >>= displayStates . (>>= next) . next . (, First) . parse
            -- readBuilding >>= displayStates . (next <=< parseState)
            -- readBuilding >>= displayStates . (next <=< next <=< parseState)
            readBuilding "example" >>= (print . length) . (next <=< next <=< parseState)
            putStrLn "==================="
            day11

printLengths :: String -> IO ()
printLengths s = mapM_ f [1..7]
    where f i =  readBuilding s >>= (print . length) . (foldr (<=<) return (replicate i next) <=< parseState)

display :: Building -> IO ()
display = putStrLn . unlines . map show . reverse

displayStates :: [State] -> IO ()
displayStates = putStrLn . unlines . map (unlines . map show . reverse . fst)

-- ============================================================================
-- | for testing in the repl
-- ============================================================================

example :: IO String
example = readFile "data/example11.txt"

-- | Starting state from text example
exampleState :: State
exampleState = ([(First, [("hydrogen",Chip),("lithium",Chip)]),
                 (Second, [("hydrogen",Generator)]),
                 (Third, [("lithium",Generator)]),
                 (Fourth, [])
                 ], First)

buildingInput :: Building
buildingInput = [(First,[("polonium",Generator),("thulium",Generator),("thulium",Chip),("promethium",Generator),("ruthenium",Generator),("ruthenium",Chip),("cobalt",Generator),("cobalt",Chip)]),(Second,[("polonium",Chip),("promethium",Chip)]),(Third,[]),(Fourth,[])]

-- ghci> (filter stateSafe) . next . (\x -> (x, First)) . parse <$> readFile "data/input11.txt" >>= displayStates 
-- ghci> (>>= next) . return . head . (>>= next) . (>>= next) . return . (!! 3) . (>>= next) . return . head . (>>= next) . next . (\x -> (x, First)) . parse <$> readFile "data/example11.txt" >>= displayStates 
