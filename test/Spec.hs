import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2

goldStar :: (HasCallStack, Example a) => a -> SpecWith (Arg a)
goldStar = specify "GOLD STAR *"

main :: IO ()
main = hspec $ do
  describe "1_1" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_1.solve input `shouldBe` 67622
  describe "1_2" $ do
    goldStar $ do
      input <- readFile "test/AOC1.txt"
      AOC1_2.solve input `shouldBe` 201491
  describe "2_1" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_1.solve input `shouldBe` 13565
  describe "2_2" $ do
    goldStar $ do
      input <- readFile "test/AOC2.txt"
      AOC2_2.solve input `shouldBe` 12424