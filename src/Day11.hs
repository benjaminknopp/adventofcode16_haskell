-- | Day 11: RTG
module Day11 where
import Prelude hiding (floor)
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad

data Type = Chip | Generator deriving ( Show, Eq, Ord )
type Element = String
type Item = (Element, Type)
type Cargo = Set Item

data Level = First | Second | Third | Fourth deriving ( Show, Enum, Eq, Ord )
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
parse = zip [First .. Fourth] . map (Set.fromList . foldr (filterItems . toItem ) [] .  splitOn "a ") . lines
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
    | Set.null (Set.filter ((== Generator) . snd) xs) = True
    | otherwise = let chips = Set.filter ((Chip ==) . snd) xs
                      gens = Set.filter ((Generator ==) . snd) xs
                      isPowered :: Item -> Bool
                      isPowered c = fst c `elem` Set.map fst gens
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
              | level == from = (level, items `Set.difference` cargo')
              | level == to = (level, items `Set.union` cargo')
              | otherwise = (level, items)

          elevator = snd state
          elevator' = dir elevator
          building = fst state
          building' = updateBuilding cargo elevator elevator' building

nextAll :: State -> [State]
nextAll state@(building, elevator@First) = _generateNextStates building elevator state up up
nextAll state@(building, elevator@Fourth) = [move (Set.singleton item) down state | item <- Set.toList floorItems]
    where 
          floorItems = snd $ building !! fromEnum elevator
nextAll state@(building, elevator) = _generateNextStates building elevator state up down

next :: State -> [State]
next = filter stateSafe . nextAll

_generateNextStates :: Building -> Level -> State -> Direction -> Direction -> [State]
_generateNextStates building elevator state dir1 dir2 = 
    [move (Set.fromList [floorItems !! i, floorItems !! j]) dir1 state | i <- [0..n-1], j <- [0..n-1], i < j]
    ++ [move (Set.singleton item) dir2 state | item <- floorItems]
    where 
        floorItems = Set.toList $ snd $ building !! fromEnum elevator
        n = length floorItems

-- | check for final state
solved :: State -> Bool
solved (building, _) = n == (length . snd) (building !! fromEnum Fourth)
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

diRec' :: Int -> Set.Set State -> [State] -> Int
diRec' n visited states
    | any solved states = n
    | otherwise = diRec' (n+1) (Set.union statesSet visited) (notVisited  >>= next)
    where statesSet = Set.fromList states
          notVisited = Set.toList (Set.difference statesSet visited)

day11 :: IO ()
day11 = do
    -- input <- fmap parse example
    input <- parse <$> readFile "data/input11.txt"
    -- input <- parse <$> readFile "data/example11.txt"
    let start = (input, First)
    -- let n = diRec 0 $ return start
    let n = diRec' 0 Set.empty $ return start
    print n
    -- let x = takeWhile (not . solve11' start) [30..]
    -- print x

display :: Building -> IO ()
display = putStrLn . unlines . map show . reverse

displayStates :: [State] -> IO ()
displayStates = putStrLn . unlines . map (unlines . map show . reverse . fst)

-- ============================================================================
-- | for testing in the repl
-- ============================================================================

example :: IO String
example = readFile "data/example11.txt"

-- ghci> (filter stateSafe) . next . (\x -> (x, First)) . parse <$> readFile "data/input11.txt" >>= displayStates 
-- ghci> (>>= next) . return . head . (>>= next) . (>>= next) . return . (!! 3) . (>>= next) . return . head . (>>= next) . next . (\x -> (x, First)) . parse <$> readFile "data/example11.txt" >>= displayStates 
