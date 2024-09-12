module Day12 where
import Data.List.Split ( splitOn )

-- type Cmd = [String]
data Reg = A | B | C | D
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
-- eval :: Cmd -> State -> State
-- parse :: String -> Cmd
