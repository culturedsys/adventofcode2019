module Day2 (
    parser,
    result,
    result2,
) where

import Data.List.Split (splitOn)

import IntCode (execute, Error)


result :: [Int] -> Either Error Int
result input = executeProgram input 12 2


executeProgram :: [Int] -> Int -> Int -> Either Error Int
executeProgram input noun verb =
    let program = (head input) : [noun, verb] ++ (drop 3 input) in
        head <$> execute program


searchPrograms :: [Int] -> Int -> [(Int, Int)]
searchPrograms input target = do
    noun <- [0..99]
    verb <- [0..99]
    case executeProgram input noun verb of
        Left error -> []
        Right result -> if result == target then [(noun, verb)] else []


result2 :: Int -> [Int] -> (Int, Int)
result2 target input = head $ searchPrograms input target

parser :: String -> [Int]
parser = map read . (splitOn ",")