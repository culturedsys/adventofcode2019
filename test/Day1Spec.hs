import Test.Hspec
import Day1(fuelRequirement, fuelRequirementWithFuel, result)

main :: IO ()
main = hspec $ do
    describe "fuelRequirement" $ do
        it "should pass case 1" $ do
            fuelRequirement 12 `shouldBe` 2
        it "should pass case 2" $ do
            fuelRequirement 14 `shouldBe` 2
        it "should pass case 3" $ do
            fuelRequirement 1969 `shouldBe` 654
        it "should pass case 4" $ do
            fuelRequirement 100756 `shouldBe` 33583
            
    describe "result" $ do
        it "should give correct result" $ do
            result [12, 14, 1969, 100756] `shouldBe` (2 + 2 + 654 + 33583)

    describe "fuelRequirementWithFuel" $ do
        it "should pass case 1" $ do
            fuelRequirementWithFuel 12 `shouldBe` 2
        it "should pass case 2" $ do
            fuelRequirementWithFuel 1969 `shouldBe` 966
        it "should pass case 3" $ do
            fuelRequirementWithFuel 100756 `shouldBe` 50346
    