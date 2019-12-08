module Day4Main where

import Day4 (result, result2)

main :: IO ()
main = do
    print $ result [168630..718098]
    print $ result2 [168630..718098]