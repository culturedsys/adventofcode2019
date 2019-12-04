module Day3 (
    Direction (..),
    Point (..),
    closestIntersection,
    shortestIntersection,
    pointsOnPath,
    parser
) where

import qualified Data.Set as S
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Direction = R Int | D Int | L Int | U Int

getLen :: Direction -> Int
getLen (R x) = x
getLen (D x) = x
getLen (L x) = x
getLen (U x) = x

data Point = Point Int Int Int deriving (Show)

instance Eq Point where
    (Point x1 y1 _) == (Point x2 y2 _) = x1 == x2 && y1 == y2 

instance Ord Point where
    compare (Point x1 y1 _) (Point x2 y2 _) = compare (x1, y1) (x2, y2)

type Points = S.Set Point

move :: Direction -> Point -> Point
move (R _) (Point x y c) = Point (x + 1) y (c + 1)
move (D _) (Point x y c) = Point x (y - 1) (c + 1)
move (L _) (Point x y c) = Point (x - 1) y (c + 1)
move (U _) (Point x  y c) = Point x (y + 1) (c + 1)


addSteps :: (Points, Point) -> Direction -> (Points, Point)
addSteps (acc, origin) direction = 
    let len = getLen direction
        points = take len . tail $ iterate (move direction) origin
        end = last points
      in
        (S.union acc $ S.fromList points, end)


pointsOnPath :: [Direction] -> Points
pointsOnPath path = 
    fst $ foldl addSteps (S.empty, Point 0 0 0) path


manhattanDistance :: Point -> Int
manhattanDistance (Point x y _) = (abs x) + (abs y) 

intersectionsOf :: [Direction] -> [Direction] -> Points
intersectionsOf path1 path2 =
    let points1 = pointsOnPath path1
        points2 = pointsOnPath path2
      in
        S.intersection points1 points2


closestIntersection :: [Direction] -> [Direction] -> Int
closestIntersection path1 path2 =
    let intersections = intersectionsOf path1 path2
    in
        head . sort . map manhattanDistance $ S.toList intersections


lookupS :: Ord a => a -> S.Set a -> Maybe a
lookupS target set = do
    result <- S.lookupGE target set
    if result == target then return result else Nothing


shortestIntersection :: [Direction] -> [Direction] -> Int
shortestIntersection path1 path2 = 
    let points1 = pointsOnPath path1
        points2 = pointsOnPath path2
        intersections = S.intersection points1 points2
        combinedDistance point = fromJust $ do
            Point x y count1 <- lookupS point points1
            Point _ _ count2 <- lookupS point points2
            return (count1 + count2) 
      in
        head . sort . S.toList $ S.map combinedDistance intersections


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