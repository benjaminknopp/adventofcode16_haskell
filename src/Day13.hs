module Day13 where

range :: Int -> [Int]
range 0 = []
range n = n : range (n-1)

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
