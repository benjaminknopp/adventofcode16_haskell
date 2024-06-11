module Main where

import Day01 (firstDuplicate, shortestDistance)
import Day02 (generateCode1, generateCode2)
import Day03 (addSides, toVert)
import Day04 (decipher, splitDash, sumIds, searchNorth)
import Day05 (getPassword, solutionB, first5zero, hash)

main :: IO ()
main = do
    day01
    day02
    day03
    day04
    day05

day05 :: IO ()
day05 = do
    let x = getPassword "ffykfhsq" -- "abc"
    print x
    let x' = solutionB $ filter first5zero $ map (hash "ffykfhsq") [1..]
    print x'

day04 :: IO ()
day04 = do
    content <- readFile "data/input04.txt"
    let y = map (decipher . splitDash) $ lines content
        result = sumIds y
        result2 = filter searchNorth y
    print result
    print result2

day03 :: IO ()
day03 = do
    input <- readFile "data/input03.txt"
    let lst = map ((\x-> map read x :: [Int] ) . words) (lines input)
    print $ sum $ map addSides lst
    print $ sum $ map addSides $ toVert lst

day02 :: IO ()
day02 = do
    input <- readFile "data/input02.txt"
    putStrLn (generateCode1 input)
    putStrLn (generateCode2 input)

day01 :: IO ()
day01 = do
    input <- readFile "data/input01.txt"
    putStrLn $ shortestDistance input
    putStrLn $ firstDuplicate input
