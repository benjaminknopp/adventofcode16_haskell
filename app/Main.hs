module Main where

import Day01 (firstDuplicate, shortestDistance)
import Day02 (generateCode1, generateCode2)
import Day03 (addSides, toVert)
import Day04 (decipher, splitDash, sumIds, searchNorth)
import Day05 (getPassword, solutionB, first5zero, hash)
import qualified Day06 (toVert)
import Day06 (count)
import Day07 (supportsTLS, supportsSSL)
import Day08 ( State(State), parse, next )
import Day09 ( duRek, insertTree, evalNode )
import Day10 ( solve )
import Day11 ( day11 )

main :: IO ()
main = do
    putStrLn "Solution of Day01:"
    day01
    putStrLn "Solution of Day02:"
    day02
    putStrLn "Solution of Day03:"
    day03
    putStrLn "Solution of Day04:"
    day04
    putStrLn "Solution of Day05 (this takes a while):"
    day05
    putStrLn "Solution of Day06:"
    day06
    putStrLn "Solution of Day07:"
    day07
    putStrLn "Solution of Day08:"
    day08
    putStrLn "Solution of Day09:"
    day09
    putStrLn "Solution of Day10:"
    day10
    putStrLn "Solution of Day11 (only part one because part two takes very long):"
    day11

day10 :: IO ()
day10 = readFile "data/input10.txt" >>= (print . (solve . lines))

day09 :: IO ()
day09 = do
    x <- readFile "data/input09.txt"
    let (_, result) = duRek (init x, 0)
    let result2 = evalNode $ insertTree $ init x
    print result
    print result2

day08 :: IO ()
day08 = do
    x <- readFile "data/input08.txt"
    let cmdStr = lines x
        cmds =  map (parse . words) cmdStr
        emptyField = State [['.' | _<-[0..49::Int]] | _<-[0..5::Int]] -- part two
        result = foldl next emptyField cmds
        State arr = result
        count' = foldl (\i c -> if c == '#' then i+1 else i) 0 $ concat arr :: Int
    print count' 
    print result

day07 :: IO ()
day07 = do
    x <- readFile "data/input07.txt"
    let result = length $ filter supportsTLS (lines x)
    let result2 = length $ filter supportsSSL (lines x)
    print result
    print result2

day06 :: IO ()
day06 = do
    x <- readFile "data/input06.txt"
    let result = map (fst . head . count) $ Day06.toVert $ lines x
    let result2 = map (fst . last . count) $ Day06.toVert $ lines x
    print result
    print result2

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
