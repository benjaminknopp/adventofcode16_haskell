module Day09 where

type Command = (Int, Int)
data Node = Plus Node Node | Mul Node Node | Leaf Int deriving ( Show )

testIn :: String
testIn = "ADVENTA(1x5)BC(3x3)XYZA(2x2)BCD(2x2)EFG(6x1)(1x3)AX(8x2)(3x3)ABCY"
testOut :: String
testOut = "ADVENTABBBBBCXYZXYZXYZABCBCDEFEFG(1x3)AX(3x3)ABC(3x3)ABCY"

-- (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN
-- 3x ((3x3)ABC(2x3)XY(5x2)PQRST) + X(18x9)(3x2)TWO(5x7)SEVEN
-- 3x (9+6+10) + 1 + ((18x9)(3x2)TWO(5x7)SEVEN)
-- 3x (9+6+10) + 1 + (9x ((3x2)TWO(5x7)SEVEN))
-- 3x (9+6+10) + 1 + (9x (6+35))

main :: IO ()
main = do
    x <- getContents 
    let (_, result) = duRek (init x, 0)
    let result2 = evalNode $ insertTree $ init x
    print result
    print result2

duRek :: (String, Int) -> (String, Int)
duRek ("", acc) = ("", acc)
duRek ('(':xs, acc) =
    let (cmd, rest) = break (==')') xs
        (l, r) = parse $ removeBrackets cmd 
        s = tail rest
    in duRek (drop l s,  r*l+acc)
duRek (xs, acc) =
    let (h, t) = break (=='(') xs
    in duRek (t, acc + length h)

-- | turns String into Node
-- >>> insertTree "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
-- Plus (Mul (Leaf 3) (Plus (Mul (Leaf 3) (Leaf 3)) (Plus (Mul (Leaf 3) (Leaf 2)) (Mul (Leaf 2) (Leaf 5))))) (Plus (Leaf 1) (Mul (Leaf 9) (Plus (Mul (Leaf 2) (Leaf 3)) (Mul (Leaf 7) (Leaf 5)))))
insertTree :: String -> Node
insertTree ('(':xs) = 
    let (cmdStr, restStr) = break (==')') xs
        (l, r) = parse cmdStr
        rest = tail restStr
    in if length rest == l 
       then Mul (Leaf r) (insertTree rest) 
       else Plus (Mul (Leaf r) (insertTree (take l rest))) (insertTree (drop l rest))
insertTree xs
    | '(' `notElem` xs = Leaf $ length xs
    | otherwise = let (h, t) = break (=='(') xs 
                  in Plus (insertTree h) (insertTree t)

evalNode :: Node -> Int
evalNode (Leaf a) = a 
evalNode (Mul a b) = evalNode a * evalNode b
evalNode (Plus a b) = evalNode a + evalNode b


parse :: String -> Command
parse s = (l, r)
    where (lStr, rStr) = break (== 'x') s
          l = read lStr
          r = read $ tail rStr

removeBrackets :: String -> String
removeBrackets = foldr (\c s -> if c `notElem` ['(',')'] then  c:s else s) ""
