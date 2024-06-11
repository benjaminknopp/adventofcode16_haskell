module Main where

import Day01 (firstDuplicate, shortestDistance)
-- import Day02 (generateCode)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ shortestDistance input
    putStrLn $ firstDuplicate input
    -- putStrLn (generateCode input)
