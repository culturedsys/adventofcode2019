module Day1Main where

import Day1 (result, resultWithFuel)
import Lib (execute)

main :: IO ()
main = do
    execute result "day1.txt"
    execute resultWithFuel "day1.txt"