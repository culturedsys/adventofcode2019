module Day3Main where

import Day3
import Lib

main :: IO ()
main = do
    execute (uncurry closestIntersection) parser "day3.txt"
    execute (uncurry shortestIntersection) parser "day3.txt"