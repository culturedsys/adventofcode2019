module Lib (
    execute
) where

import System.FilePath ((</>))

execute :: (Read a, Show b) => ([a] -> b) -> String -> IO ()
execute result fileName = do
    lines <- lines <$> readFile ("input" </> fileName)
    let input = map read lines
    putStrLn .show $  result input