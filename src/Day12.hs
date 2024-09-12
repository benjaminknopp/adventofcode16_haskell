module Day12 where
import Data.List.Split ( splitOn )
import Data.Map (Map)
import qualified Data.Map as Map

type Reg = Char  -- 'a' | 'b' | 'c' | 'd'
data IntOrReg = Value Int | Register Reg
data Cmd = Cpy IntOrReg Reg
         | Inc Reg
         | Dec Reg 
         | Jnz IntOrReg Int

-- data Registers = Registers {a::Int
--                           , b::Int
--                           , c::Int
--                           , d::Int} deriving (Show)
type Registers = Map Reg Int
type Tape = [Cmd]
type Index = Int
type State = (Index, Registers)


initialReg :: Registers
initialReg = Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)]

day12 :: IO ()
day12 = readFile "data/example12.txt" >>= (print . map (splitOn " ") . lines)

-- run :: [Cmd] -> Registers
-- run _ = Registers 1 2 3 4

-- solve :: Tape -> State -> State
-- next :: State -> State
-- read :: Tape -> Index -> Cmd
eval :: Cmd -> State -> State
eval (Cpy x y) (i, r) = (0, initialReg)
eval (Inc  c) s = inc c s
eval (Dec c) s = dec c s
eval (Jnz x y) s = jnz x y s
-- parse :: String -> Cmd

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

