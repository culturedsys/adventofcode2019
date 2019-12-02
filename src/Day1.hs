module Day1 (
    fuelRequirement,
    fuelRequirementWithFuel,
    result,
    resultWithFuel
) where

fuelRequirement :: Integer -> Integer
fuelRequirement mass = (mass `div` 3) - 2 

result :: [Integer] -> Integer
result = sum . map fuelRequirement 

fuelRequirementWithFuel :: Integer -> Integer
fuelRequirementWithFuel mass = 
    sum . takeWhile (> 0) . drop 1 $ iterate fuelRequirement mass 

resultWithFuel:: [Integer] -> Integer
resultWithFuel = sum . map fuelRequirementWithFuel