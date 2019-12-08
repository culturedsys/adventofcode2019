module Day4 (
    check, twoOrMore, exactlyTwo, result, result2
) where

import Data.List (groupBy)
import Debug.Trace

checkNotDescending Nothing _ = True
checkNotDescending (Just previous) group = previous <= (head group)


twoOrMore group = (length group) >= 2


exactlyTwo group = (length group) == 2


check :: ([Int] -> Bool) -> Int -> Bool
check groupCondition value = hasPair && notDescending
  where
    (_, hasPair, notDescending) = foldl (\(previous, hasPair, notDescending) group -> 
        (Just $ head group, hasPair || groupCondition group, notDescending && (checkNotDescending previous group)))
        (Nothing, False, True) groups
    groups = groupBy (==) (reverse $ splitDigits value)
    splitDigits x | x < 10 = [x]
    splitDigits x = (x `mod` 10) : (splitDigits (x `div` 10))


result :: [Int] -> Int
result = length . filter (check twoOrMore)


result2 :: [Int] -> Int
result2 = length . filter (check exactlyTwo)