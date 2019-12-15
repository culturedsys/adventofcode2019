module Day6 (
    result,
    orbitsToTree,
    countPaths,
    parse,
    Tree(..)
) where

import qualified Data.MultiMap as M
import Data.List.Split (splitOn)

data Tree = Tree String [Tree] deriving (Show, Eq)


orbitsToTree :: [(String, String)] -> Tree
orbitsToTree orbits =
    let orbitMap = M.fromList orbits
        treeAt name = 
            let orbitors = M.lookup name orbitMap
                children = map treeAt orbitors
              in Tree name children
      in treeAt "COM"


countPaths :: Tree -> Int
countPaths = countPaths' 0 where
    countPaths' depth (Tree _ []) = depth
    countPaths' depth (Tree _ children) = depth + (sum $ map (countPaths' (depth + 1)) children)


result :: [(String, String)] -> Int
result orbits = 
    let tree = orbitsToTree orbits
      in countPaths tree


parse :: String -> [(String, String)]
parse input =
    let ls = lines input
        parseLine l = let [a, b] = splitOn ")" l in (a, b)
      in map parseLine ls