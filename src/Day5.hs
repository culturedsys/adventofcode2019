module Day5 where

import Data.List.Split
import IntCode

result :: Int -> [Int] -> Either Error [Int]
result id program = executeWithIo program [id]


parse :: String -> [Int]
parse input = map read (splitOn "," input)