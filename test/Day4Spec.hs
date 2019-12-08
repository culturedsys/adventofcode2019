module Day4Spec where

import Test.Hspec

import Day4 (check, twoOrMore, exactlyTwo)

main :: IO ()
main = hspec $ do
    describe "check" $ do
        it "should pass the first case" $ do
            check twoOrMore 111111 `shouldBe`  True
        it "should pass the second case" $ do
            check twoOrMore 223450 `shouldBe` False
        it "should pass the third case" $ do
            check twoOrMore 123789 `shouldBe` False

    describe "check2" $ do
        it "should pass the first case" $ do
            check exactlyTwo 112233 `shouldBe` True
        it "should pass the second case" $ do
            check exactlyTwo 123444 `shouldBe` False
        it "should pass the third case" $ do
            check exactlyTwo 111122 `shouldBe` True
        it "should pass the second case" $ do
            check exactlyTwo 223450 `shouldBe` False
    