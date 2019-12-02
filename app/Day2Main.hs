module Day2Main where

import Data.List.Split (splitOn)

import Day2 (result, result2)
import Lib (execute)

main :: IO ()
main = do 
    execute result (splitOn ",") "day2.txt"
    execute (result2 19690720) (splitOn ",") "day2.txt"