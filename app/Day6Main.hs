module Day6Main where

import Day6
import Lib


main = do
    execute orbitCountChecksum parse "day6.txt"
    execute (orbitalTransfers "SAN" "YOU") parse "day6.txt"