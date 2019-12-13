module IntCodeSpec where

import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck

import IntCode (loadMemory, execute, executeWithIo)

main = hspec $ do
    describe "loadMemory" $ do
        it "should load correctly" $ do
            loadMemory [1, 3, 5] `shouldBe` M.fromList [(0, 1), (1, 3), (2, 5)]

    describe "execute" $ do
        it "should pass case 1" $ do
            execute [1, 0, 0, 0, 99] `shouldBe` Right [2,0,0,0,99]
        it "should pass case 2" $ do
            execute [2,3,0,3,99] `shouldBe` Right [2,3,0,6,99]
        it "should pass case 3" $ do
            execute [2,4,4,5,99,0] `shouldBe` Right [2,4,4,5,99,9801]
        it "should pass case 4" $ do
            execute [1,1,1,4,99,5,6,0,99] `shouldBe` Right [30,1,1,4,2,5,6,0,99]

    describe "executeWithIo" $ do
        it "should pass case 1" $ property $ do
            \ x -> executeWithIo [3, 0, 4, 0, 99] [x] == (Right [x])
    