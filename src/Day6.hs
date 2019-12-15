module Day6 (
    orbitCountChecksum,
    orbitalTransfers,
    orbitsToTree,
    countPaths,
    parse,
    findPath,
    Tree(..)
) where

import qualified Data.MultiMap as M
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, listToMaybe, fromJust)


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


orbitCountChecksum :: [(String, String)] -> Int
orbitCountChecksum orbits = 
    let tree = orbitsToTree orbits
      in countPaths tree



findPath' :: String -> Tree -> Maybe [String]
findPath' label (Tree current children) = 
    if label == current then 
        Just []
    else
        case listToMaybe $ mapMaybe (findPath' label) children of
            Nothing -> Nothing
            Just rest -> Just (current : rest)


findPath :: String -> Tree -> [String]
findPath label tree = fromJust $ findPath' label tree


orbitalTransfers :: String -> String -> [(String, String)] -> Int
orbitalTransfers source dest orbits = 
    let tree = orbitsToTree orbits
        sourcePath = findPath source tree
        destPath = findPath dest tree
        commonPath = takeWhile (\(a, b) -> a == b) $ zip sourcePath destPath
     in
        ((length sourcePath) + (length destPath)) - 2 * (length commonPath)


parse :: String -> [(String, String)]
parse input =
    let ls = lines input
        parseLine l = let [a, b] = splitOn ")" l in (a, b)
      in map parseLine ls