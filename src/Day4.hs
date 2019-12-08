{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (
    check, check2, result, result2
) where

import Data.List (groupBy)
import Debug.Trace


check :: Int -> Bool
check value = hasPair && notDescending
  where
    (_, hasPair, notDescending) = foldl (\(previous, hasPair, notDescending) group -> 
        (Just $ head group, hasPair || (length group) >= 2, notDescending && (checkNotDescending previous group)))
        (Nothing, False, True) groups
    groups = groupBy (==) (reverse $ splitDigits value)
    splitDigits x | x < 10 = [x]
    splitDigits x = (x `mod` 10) : (splitDigits (x `div` 10))
    checkNotDescending Nothing _ = True
    checkNotDescending (Just previous) group = previous <= (head group)
    

result :: [Int] -> Int
result = length . filter check


check2 :: Int -> Bool
check2 value = hasPair && notDescending
  where
    (_, hasPair, notDescending) = foldl (\(previous, hasPair, notDescending) group -> 
        (Just $ head group, hasPair || (length group) == 2, notDescending && (checkNotDescending previous group)))
        (Nothing, False, True) groups
    groups = groupBy (==) (reverse $ splitDigits value)
    splitDigits x | x < 10 = [x]
    splitDigits x = (x `mod` 10) : (splitDigits (x `div` 10))
    checkNotDescending Nothing _ = True
    checkNotDescending (Just previous) group = previous <= (head group)
    

result2 :: [Int] -> Int
result2 = length . filter check2