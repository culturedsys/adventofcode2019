module Day1 (
    fuelRequirement,
    result
) where

fuelRequirement :: Integer -> Integer
fuelRequirement mass = (mass `div` 3) - 2 

result :: [Integer] -> Integer
result = sum . map fuelRequirement 