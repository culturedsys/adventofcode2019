module Lib (
    execute
) where

import System.FilePath ((</>))

execute :: (Read a, Show b) => ([a] -> b) -> (String -> [String]) -> String -> IO ()
execute result splitter fileName = do
    lines <- splitter <$> readFile ("input" </> fileName)
    let input = map read lines
    putStrLn .show $  result input