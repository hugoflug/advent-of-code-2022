import Test.Hspec
import qualified AOC1_1
import qualified AOC1_2
import qualified AOC2_1
import qualified AOC2_2
import qualified AOC3_1
import qualified AOC3_2
import qualified AOC4_1
import qualified AOC4_2

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
  describe "3_1" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_1.solve input `shouldBe` 7727
  describe "3_2" $ do
    goldStar $ do
      input <- readFile "test/AOC3.txt"
      AOC3_2.solve input `shouldBe` 2609
  describe "4_1" $ do
    goldStar $ do
      input <- readFile "test/AOC4.txt"
      AOC4_1.solve input `shouldBe` 644
  describe "4_2" $ do
    goldStar $ do
      input <- readFile "test/AOC4.txt"
      AOC4_2.solve input `shouldBe` 926