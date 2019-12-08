module Day4 (
    check, result
) where

check :: Int -> Bool
check value = hasPair && notDescending
  where
    (_, hasPair, notDescending) = foldl (\(previous, hasPair, notDescending) digit -> 
        (digit, hasPair || checkPair previous digit, notDescending && checkNotDescending previous digit))
        (head digits, False, True) (tail digits)
    digits = reverse $ splitDigits value
    splitDigits x | x < 10 = [x]
    splitDigits x = (x `mod` 10) : (splitDigits (x `div` 10))
    checkPair previous digit = previous == digit
    checkNotDescending previous digit = previous <= digit
    

result :: [Int] -> Int
result = length . filter check