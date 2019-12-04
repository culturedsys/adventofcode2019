module Day3Spec where

import qualified Data.Set as S
import Test.Hspec

import Day3

main :: IO () 
main = hspec $ do
    describe "pointsOnPath" $ do
        it "should add one point" $ do
            pointsOnPath [R 1] `shouldBe` S.singleton (1, 0)

    describe "closestIntersection" $ do
        it "should pass case 1" $ do
            closestIntersection [R 8, U 5, L 5, D 3] [U 7, R 6, D 4, L 4] `shouldBe` 6
        it "should pass case 2" $ do
            closestIntersection [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72]
                [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83] `shouldBe` 159
        it "should pass case 3" $ do
            closestIntersection [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51]
                [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7] `shouldBe` 135