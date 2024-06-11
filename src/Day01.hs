module Day01 where
import Data.List.Split ( splitOn )

-- data representation for left right continue
data Turn = L | R | C
type Distance = Int
type Instruction = (Turn, Distance)
type Position = (Int, Int)
data Heading = N | E | S | W
type State = (Heading, Position)

shortestDistance :: String -> String
shortestDistance s = let (a, b) = (position . path) s in
    show $ abs a + abs b

firstDuplicate :: String -> String
firstDuplicate s = 
    let instructions = strToInstruction s
        cpath = crawl instructions (N, (0,0)) []
        (x, y) = case firstRevisit cpath of
            Just (x', y') -> (x', y')
            Nothing -> error ""
    in show $ abs x + abs y

path :: String -> [State]
path = scanl move (N, (0, 0)) . strToInstruction

strToInstruction :: String -> [Instruction]
strToInstruction = map toInstruction . parse

-- R8, R4, R4, R8 -> (0, 4)
position :: [State] -> Position
position = snd . last

crawl :: [Instruction] -> State -> [Position] -> [Position]
crawl [] _ positions = positions
crawl ((_,0):xs) state positions = crawl xs state positions
crawl (x:xs) state positions =
    let heading = turn (fst state) (fst x)
        pos = walk heading 1 (snd state)
    in crawl ((C, snd x - 1):xs) (heading, pos) (positions ++ [pos])

firstRevisit :: [Position] -> Maybe Position
firstRevisit [] = Nothing
firstRevisit (x:xs) = if x `elem` xs then Just x else firstRevisit xs

move :: State -> Instruction -> State
move s c = let h = turn (fst s) (fst c) in
    (h, walk h (snd c) (snd s))

walk :: Heading -> Distance -> Position -> Position
walk N d p = (fst p, snd p + d)
walk S d p = (fst p, snd p - d)
walk E d p = (fst p + d, snd p)
walk W d p = (fst p - d, snd p)

turn :: Heading -> Turn -> Heading
turn N R = E
turn N L = W
turn E R = S
turn E L = N
turn S R = W
turn S L = E
turn W R = N
turn W L = S
turn x C = x

parse :: String -> [String]
parse = splitOn ", "

toTurn :: Char -> Turn
toTurn x = case x of
    'R' -> R
    'L' -> L
    _ -> error "No valid Turn"

toInstruction :: String -> Instruction
toInstruction s = (toTurn $ head s, read $ drop 1 s )
