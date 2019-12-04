module Day1Main where

import Day1 (result, resultWithFuel, parser)
import Lib (execute) 

main :: IO ()
main = do
    execute result parser "day1.txt"
    execute resultWithFuel parser "day1.txt" 