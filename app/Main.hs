module Main where

import Day01 (firstDuplicate, shortestDistance)
import Day02 (generateCode1, generateCode2)
import MyLib (getInput)

main :: IO ()
main = do
    day01
    day02

day02 :: IO ()
day02 = do
    input <- getInput "data/input02.txt"
    putStrLn (generateCode1 input)
    putStrLn (generateCode2 input)

day01 :: IO ()
day01 = do
    input <- getInput "data/input01.txt"
    putStrLn $ shortestDistance input
    putStrLn $ firstDuplicate input
