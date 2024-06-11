module Day08 where

type Array = [String]
newtype State = State Array
instance Show State where
    show (State array) = unlines array
type Index = Int
data Axis = Row | Col deriving Show
data Cmd = Rect Index Index | Rotate Axis Index Index deriving Show

splitBy   :: Char -> String -> [String]
splitBy sep s =  case dropWhile (sep==) s of
                      "" -> []
                      s' -> w : splitBy sep s''
                            where (w, s'') = break (sep==) s'

-- parse list of words
parse :: [String] -> Cmd
parse [_, b] = 
    let [i, j] = map read $ splitBy 'x' b :: [Int]
    in Rect i j
parse [_, axisStr, iStr, _, offs] = 
    let [_, i] = splitBy '=' iStr
        axis = if axisStr == "row" then Row else Col
    in Rotate axis (read i) (read offs)
parse _ = error "parsing failed"

-- use this for folding
next :: State -> Cmd -> State
next s (Rect i j) = rect s i j
next s (Rotate ax c o) = rotate s ax c o

-- | create rectangle
-- >>> rect empty 3 2
-- ex001
rect :: State -> Index -> Index -> State
rect (State arr) j i = State $ createArr f m n
    where m = length arr
          n = length $ head arr
          f i' j' = if i' < i && j' < j then '#' else arr!!i'!!j'

createArr :: (Index -> Index -> Char) -> Index -> Index -> Array
createArr f m n = [[f i' j' | j' <- [0..(n-1)]] | i' <- [0..(m-1)]]

rotate :: State -> Axis -> Index -> Index -> State
rotate (State arr) Row i offs = State $ createArr f m n
    where n = length $ head arr
          m = length arr
          f i' j' = if i' /= i then arr!!i'!!j' else arr!!i'!!((j'-offs)`mod` n)
rotate (State arr) Col j offs = State $ createArr f m n
    where n = length $ head arr
          m = length arr
          f i' j' = if j' /= j then arr!!i'!!j' else arr!!((i'-offs)`mod`m)!!j'

empty :: State
empty = State [".......",
               ".......",
               "......."]

ex001 :: State
ex001 = State ["###....",
               "###....",
               "......."]
ex002 :: State
ex002 = State ["#.#....",
               "###....",
               ".#....."]
ex003 :: State
ex003 = State ["....#.#",
               "###....",
               ".#....."]
ex004 :: State
ex004 = State [".#..#.#",
               "#.#....",
               ".#....."]
