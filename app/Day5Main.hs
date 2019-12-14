module Day5Main where

import Data.List.Split

import Lib
import Day5


main :: IO () 
main = do
    execute (result 1) parse "day5.txt"
    execute (result 5) parse "day5.txt"