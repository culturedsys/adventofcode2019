module Day2Main where

import Day2 (result, result2, parser)
import Lib (execute)

main :: IO ()
main = do 
    execute result parser "day2.txt"
    execute (result2 19690720) parser "day2.txt"