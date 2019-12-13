module Day2Spec where

import qualified Data.Map as M

import Test.Hspec

import Day2 (result)

main = hspec $ do
    describe "result" $ do
        it "should return the first result" $ do
            result [1, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 12] `shouldBe` Right 14