module Day12 where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)

type Reg = Char  -- 'a' | 'b' | 'c' | 'd'
data IntOrReg = Value Int | Register Reg 
    deriving (Show)
data Cmd = Cpy IntOrReg Reg
         | Inc Reg
         | Dec Reg 
         | Jnz IntOrReg Int
    deriving (Show)

type Registers = Map Reg Int
type Tape = [Cmd]
type Index = Int
type State = (Index, Registers)

day12 :: Map Char Int -> IO ()
day12 initialReg = do
    f <- readFile "data/input12.txt"
    let tape = (map parse . lines) f
    print tape
    print $ solve tape (0, initialReg)

day12a :: IO ()
day12a = day12 $ Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)] 

day12b :: IO ()
day12b = day12 $ Map.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)] 

solve :: Tape -> State -> State
solve tape s@(i, _) 
    | i >= length tape = s
    | otherwise = solve tape (eval (readTape tape i) s) 

readTape :: Tape -> Index -> Cmd
readTape tape i = tape!!i

eval :: Cmd -> State -> State
eval (Cpy x y) s = cpy x y s
eval (Inc  c) s = inc c s
eval (Dec c) s = dec c s
eval (Jnz x y) s = jnz x y s

parse :: String -> Cmd
parse s = case words s of
    ["cpy", x, y] -> Cpy (parseArg x) (head y)
    ["inc", [x]] -> Inc x
    ["dec", [x]] -> Dec x
    ["jnz", x, y] -> Jnz (parseArg x) (read y)
    _ -> error "Parsing error"

parseArg :: String -> IntOrReg
parseArg x'
    | all isDigit x' = Value (read x')
    | otherwise = Register (head x')
       

cpy :: IntOrReg -> Reg -> State -> State
cpy (Value x) c (i, m) = (i+1, Map.insert c x m)
cpy (Register x) c (i, m) = let maybeV = Map.lookup x m in
                    case maybeV of
                        Just v -> (i+1, Map.insert c v m)
                        Nothing -> error "Key Lookup Error"

inc :: Reg -> State -> State
inc c (i, m) = (i+1, Map.adjust (+ 1) c m)

dec :: Reg -> State -> State
dec c (i, m) = (i+1, Map.adjust (flip (-) 1) c m)

jnz :: IntOrReg -> Int -> State -> State
jnz (Value x) j (i, m) = if x == 0 then (i + 1, m) else (i + j, m)
jnz (Register x) j (i, m) = let maybeV = Map.lookup x m in
    case maybeV of
        Just 0 -> (i + 1, m)
        Just _ -> (i + j, m)
        Nothing -> error "Key Lookup Error"
