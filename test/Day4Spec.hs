module Day4Spec where

import Test.Hspec

import Day4 (check)

main :: IO ()
main = hspec $ do
    describe "check" $ do
        it "should pass the first case" $ do
            check 111111 `shouldBe`  True
        it "should pass the second case" $ do
            check 223450 `shouldBe` False
        it "should pass the third case" $ do
            check 123789 `shouldBe` False