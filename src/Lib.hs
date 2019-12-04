module Lib (
    execute
) where

import System.FilePath ((</>))

execute :: (Show b) => (a -> b) -> (String -> a) -> String -> IO ()
execute result parser fileName = do
    parsed <- parser <$> readFile ("input" </> fileName)
    putStrLn .show $ result parsed