module IntCodeSpec where

import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck

import IntCode (loadMemory, execute, executeWithFixedIo)

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
        it "should support addressing modes" $ do
            execute [1002, 4, 3, 4, 33] `shouldBe` Right [1002, 4, 3, 4, 99]

    describe "comparison instructions" $ do
        it "should pass case 1" $ do
            executeWithFixedIo [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [8] `shouldBe` Right [1]
            executeWithFixedIo [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] [9] `shouldBe` Right [0]
        it "should pass case 2" $ do
            executeWithFixedIo [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [7] `shouldBe` Right [1]
            executeWithFixedIo [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] [8] `shouldBe` Right [0]
        it "should pass case 3" $ do
            executeWithFixedIo [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [7] `shouldBe` Right [999]
            executeWithFixedIo [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [8] `shouldBe` Right [1000]
            executeWithFixedIo [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [9] `shouldBe` Right [1001]

    describe "jump instructions" $ do
        it "should pass case 1" $ do
            executeWithFixedIo [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [0] `shouldBe` Right [0]
            executeWithFixedIo [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] [10] `shouldBe` Right [1]
        it "should pass case 2" $ do
            executeWithFixedIo [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] [0] `shouldBe` Right [0]
            executeWithFixedIo [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] [10] `shouldBe` Right [1]


    describe "executeWithFixedIo" $ do
        it "should pass case 1" $ property $ do
            \ x -> executeWithFixedIo [3, 0, 4, 0, 99] [x] == (Right [x])
    