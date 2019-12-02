module Day1Main where

import Day1 (result, resultWithFuel)
import Lib (execute)

main :: IO ()
main = do
    execute result lines "day1.txt"
    execute resultWithFuel lines "day1.txt"