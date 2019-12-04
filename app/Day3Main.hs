module Day3Main where

import Day3
import Lib

main :: IO ()
main = execute (uncurry closestIntersection) parser "day3.txt"