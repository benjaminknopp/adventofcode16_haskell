module Day12 where
import Data.List.Split ( splitOn )

-- type Cmd = [String]
-- data Reg = A | B | C | D
type Reg = Char  -- 'a' | 'b' | 'c' | 'd'
data IntOrReg = Value Int | Register Reg
data Cmd = Cpy IntOrReg Reg
         | Inc Reg
         | Dec Reg 
         | Jnz IntOrReg Int

data Registers = Registers {a::Int
                          , b::Int
                          , c::Int
                          , d::Int} deriving (Show)
type Tape = [Cmd]
type Index = Int
type State = (Index, Registers)

day12 :: IO ()
day12 = readFile "data/example12.txt" >>= (print . map (splitOn " ") . lines)

run :: [Cmd] -> Registers
run _ = Registers 1 2 3 4

-- solve :: Tape -> State -> State
-- next :: State -> State
-- read :: Tape -> Index -> Cmd
eval :: Cmd -> State -> State
eval (Cpy x y) (i, r) = (0, Registers 0 0 0 0)
eval (Inc x) (i, r) = (0, Registers 0 0 0 0)
eval (Dec x) (i, r) = (0, Registers 0 0 0 0)
eval (Jnz x y) (i, r) = (0, Registers 0 0 0 0)
-- parse :: String -> Cmd

inc :: Reg -> State -> State
inc 'a' (i, Registers a b c d) = (i+1, Registers (a + 1) b c d)
inc 'b' (i, Registers a b c d) = (i+1, Registers a (b + 1) c d)
inc 'c' (i, Registers a b c d) = (i+1, Registers a b (c + 1) d)
inc 'd' (i, Registers a b c d) = (i+1, Registers a b c (d + 1))

dec :: Reg -> State -> State
inc 'a' (i, Registers a b c d) = (i+1, Registers (a - 1) b c d)
inc 'b' (i, Registers a b c d) = (i+1, Registers a (b - 1) c d)
inc 'c' (i, Registers a b c d) = (i+1, Registers a b (c - 1) d)
inc 'd' (i, Registers a b c d) = (i+1, Registers a b c (d - 1))
