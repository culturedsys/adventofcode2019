module Day6Spec where

import Test.Hspec

import Day6

main :: IO ()
main = hspec $ do
    describe "orbitsToTree" $ do
        it "should build simple tree" $ do
            orbitsToTree [("COM", "A"), ("COM", "B"), ("A", "C")] `shouldBe`
                Tree "COM" [Tree "A" [Tree "C" []], Tree "B" []]

    describe "countPaths" $ do
        it "should count an empty tree" $ do
            countPaths (Tree "COM" []) `shouldBe` 0
        it "should count a simple tree" $ do
            countPaths (Tree "COM" [Tree "A" [Tree "C" []], Tree "B" []]) `shouldBe` 4
        it "should count a deeper tree" $ do
            countPaths (Tree "COM" [Tree "A" [Tree "B" [Tree "C" []]]]) `shouldBe` 6
        it "shoukd count a balanced tree" $ do
            countPaths (Tree "COM" [Tree "A" [], Tree "B" []]) `shouldBe` 2
        it "shoukd count a deeper balanced tree" $ do
            countPaths (Tree "COM" [
                    Tree "A" [Tree "B" [], Tree "C" []],
                    Tree "D" [Tree "E" [], Tree "F" []]
                ]) `shouldBe` 10

    describe "orbitCountChecksum" $ do
        it "should pass case 1" $ do
            orbitCountChecksum [
                    ("COM", "B"),
                    ("B", "C"),
                    ("C", "D"),
                    ("D", "E"),
                    ("E", "F"),
                    ("B", "G"),
                    ("G", "H"),
                    ("D", "I"),
                    ("E", "J"),
                    ("J", "K"),
                    ("K", "L")
                ] `shouldBe` 42

    describe "findPath" $ do
        it "should find a short path" $ do
            findPath "B" (Tree "A" [Tree "B" []]) `shouldBe` ["A"]

    describe "orbitalTransfers" $ do
        it "should pass case 1" $ do
            orbitalTransfers "SAN" "YOU" [
                    ("COM", "B"),
                    ("B", "C"),
                    ("C", "D"),
                    ("D", "E"),
                    ("E", "F"),
                    ("B", "G"),
                    ("G", "H"),
                    ("D", "I"),
                    ("E", "J"),
                    ("J", "K"),
                    ("K", "L"),
                    ("K", "YOU"),
                    ("I", "SAN")
                ] `shouldBe` 4
