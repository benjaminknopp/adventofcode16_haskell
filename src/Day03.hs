module Day03 where
import Data.List
import GHC.Utils.Misc ( chunkList )

main :: IO ()
main = do
    content <- getContents
    let lst = map ((\x-> map read x :: [Int] ) . words) (lines content)
    print $ sum $ map addSides lst
    print $ sum $ map addSides $ toVert lst

-- | transpose list of lists
-- >>> toVert [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
toVert :: [[Int]] -> [[Int]]
toVert lol = 
    let n = length lol
    in chunkList 3 [lol!!j!!i |i<-[0..2],j<-[0..(n-1)]]

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

-- | add sides
-- >>> addSides [5, 10, 25]
-- 0
-- >>> addSides [16, 10, 25]
-- 1
-- >>> addSides [5, 10, 25000]
-- 0
addSides :: [Int] -> Int
addSides lst = boolToInt $ all (\x -> sum (take 2 x) > x!!2) (permutations lst)
