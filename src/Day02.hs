module Day02 where
import Data.List.Split
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map

type Instruction = String
type Number = Char

-- | Solve day 02 first part
--
-- Examples:
--
-- >>> generateCode "ULL\nRRDDD\nLURDL\nUUUUD\n"
-- "1985"
generateCode1 :: String -> String
generateCode1 = join . flip (getCode move) '5' . splitInstructions

-- | Solve day 02 second part
generateCode2 :: String -> String
generateCode2 = join . flip (getCode (move2 numberPadGraph)) '5' . splitInstructions

join :: [Char] -> String
join = foldr (:) ""

-- | Get code
-- >>> getCode ["ULL","RRDDD","LURDL","UUUUD"] 5
-- [1,9,8,5]
getCode :: (Char -> Number -> Number) -> [Instruction] -> Number -> [Number]
getCode _ [] _ = []
getCode mfunc (x:xs) startNum = 
    let num = getNumber x mfunc startNum
    in num : getCode mfunc xs num

-- | Split instructions
-- Examples:
-- >>> splitInstructions "ULL\nRRDDD\nLURDL\nUUUUD\n"
-- ["ULL","RRDDD","LURDL","UUUUD"]
splitInstructions :: String -> [Instruction]
splitInstructions = init . splitOn "\n"

-- | Get single number
-- >>> getNumber "ULL" 5
-- 1
-- >>> getNumber "RRDDD" 1
-- 9
-- >>> getNumber "LURDL" 9
-- 8
-- >>> getNumber "UUUUD" 8
-- 5
getNumber :: Instruction -> (Char -> Number -> Number) -> Number -> Number
getNumber instructions mfunc startNum = foldl (flip mfunc) startNum instructions

-- | Move given single instruction
-- >>> move 'U' 5
-- 2
-- >>> move 'L' 2
-- 1
-- >>> move 'L' 1
-- 1
move :: Char -> Number -> Number
move 'U' '1' = '1'
move 'R' '1' = '2'
move 'D' '1' = '4'
move 'L' '1' = '1'
move 'U' '2' = '2'
move 'R' '2' = '3'
move 'D' '2' = '5'
move 'L' '2' = '1'
move 'U' '3' = '3'
move 'R' '3' = '3'
move 'D' '3' = '6'
move 'L' '3' = '2'
move 'U' '4' = '1'
move 'R' '4' = '5'
move 'D' '4' = '7'
move 'L' '4' = '4'
move 'U' '5' = '2'
move 'R' '5' = '6'
move 'D' '5' = '8'
move 'L' '5' = '4'
move 'U' '6' = '3'
move 'R' '6' = '6'
move 'D' '6' = '9'
move 'L' '6' = '5'
move 'U' '7' = '4'
move 'R' '7' = '8'
move 'D' '7' = '7'
move 'L' '7' = '7'
move 'U' '8' = '5'
move 'R' '8' = '9'
move 'D' '8' = '8'
move 'L' '8' = '7'
move 'U' '9' = '6'
move 'R' '9' = '9'
move 'D' '9' = '9'
move 'L' '9' = '8'
move _ _ = error "illegal move"

-- Use different approach for part 2

type Neighbours = Map.Map Char Char
type NumberPadGraph = Map.Map Char Neighbours


-- Define the number pad graph
numberPadEmpty1 :: NumberPadGraph
numberPadEmpty1 = Map.fromList
  [ (x, Map.empty) | x <- ['1'..'9']]
numberPadEmpty2 :: NumberPadGraph
numberPadEmpty2 = Map.fromList
  [ (x, Map.empty) | x <- ['1'..'9'] <> ['A'..'D']]

rev :: Char -> Char
rev x 
  | x `elem` "UD" = if x == 'U' then 'D' else 'U'
  | x `elem` "LR" = if x == 'L' then 'R' else 'L'
  | otherwise = error "Invalid input"


type Connection = (Char, Char, Char)

toConnection :: String -> Connection
toConnection [a, b, c] = (a, b, c)
toConnection _ = error "Invalid Connection"

-- | Connect buttons of NumberPadGraph
-- >>> connect (Map.fromList [ (x, Map.empty) | x <- ['1'..'9'] <> ['A'..'D']]) "1D3"
--  fromList [('1',fromList [('D', '3')]),('2',fromList []),('3',fromList ['U', '1']),('4',fromList []),('5',fromList []),('6',fromList []),('7',fromList []),('8',fromList []),('9',fromList []),('A',fromList [] ),('B',fromList []),('C',fromList []),('D',fromList [])]
connect :: NumberPadGraph -> Connection -> NumberPadGraph
connect graph connection = 
    let (a, b, c) = connection
        updated = Map.insert a (Map.insert b c (graph Map.! a)) graph
        updated2 = Map.insert c (Map.insert (rev b) a (updated Map.! c)) updated
    in updated2

numberPadGraph :: NumberPadGraph
numberPadGraph = foldl connect numberPadEmpty2 $ map toConnection ["1D3"
    , "2D6"
    , "2R3"
    , "3R4"
    , "3D7"
    , "4D8"
    , "5R6"
    , "6DA"
    , "6R7"
    , "7DB"
    , "7R8"
    , "8DC"
    , "8R9"
    , "ARB"
    , "BDD"
    , "BRC"
    ]

-- Function to move on the number pad graph
move2 :: NumberPadGraph -> Char -> Char -> Char
move2 graph direction number =
  case Map.lookup number graph of
    Just neighbours -> fromMaybe number (Map.lookup direction neighbours)
    Nothing -> error "Invalid number"
