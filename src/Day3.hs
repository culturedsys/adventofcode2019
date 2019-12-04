module Day3 (
    Direction (..),
    closestIntersection
,
    pointsOnPath,
    parser
) where

import qualified Data.Set as S
import Data.List (sort)
import Data.List.Split (splitOn)

data Direction = R Int | D Int | L Int | U Int

getLen :: Direction -> Int
getLen (R x) = x
getLen (D x) = x
getLen (L x) = x
getLen (U x) = x


type Points = S.Set (Int, Int)

move :: Direction -> (Int, Int) -> (Int, Int)
move (R _) (x, y) = (x + 1, y)
move (D _) (x, y) = (x, y - 1)
move (L _) (x, y) = (x - 1, y)
move (U _) (x, y) = (x, y + 1)


addSteps :: (Points, (Int, Int)) -> Direction -> (Points, (Int, Int))
addSteps (acc, origin) direction = 
    let len = getLen direction
        points = take len . tail $ iterate (move direction) origin
        end = last points
      in
        (S.union acc $ S.fromList points, end)


pointsOnPath :: [Direction] -> Points
pointsOnPath path = 
    fst $ foldl addSteps (S.empty, (0, 0)) path


manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = (abs x) + (abs y) 


closestIntersection :: [Direction] -> [Direction] -> Int
closestIntersection path1 path2 =
    let points1 = pointsOnPath path1
        points2 = pointsOnPath path2
        intersections = S.intersection points1 points2
    in
        head . sort . map manhattanDistance $ S.toList intersections


parseDirection :: String -> Direction
parseDirection ('R' : l) = R (read l)
parseDirection ('D' : l) = D (read l)
parseDirection ('L' : l) = L (read l)
parseDirection ('U' : l) = U (read l)


parser :: String -> ([Direction], [Direction])
parser input =
    let parseLine = map parseDirection . splitOn ","
        [path1, path2] = map parseLine $ lines input
      in (path1, path2)