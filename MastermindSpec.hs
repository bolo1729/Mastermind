import Mastermind
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

possible' :: String -> [Pattern]
possible' = possible . parseTurns . words

spec :: Spec
spec = do
  describe "parseTurns" $ do
    it "parses null list" $ do
      parseTurns [] `shouldBe` []
    it "works with leading zeros" $ do
      parseTurns ["0034", "03", "56", "4"] `shouldBe` [([0,0,3,4], (0,3)), ([0,0,5,6], (0,4))]
    it "works for some examples" $ do
      parseTurns ["1234", "30", "1254", "40"] `shouldBe` [([1,2,3,4], (3,0)), ([1,2,5,4], (4,0))]
  describe "check" $ do
    it "works for some examples" $ do
      check [1,2,3,4] [1,2,3,4] `shouldBe` (4,0)
      check [1,2,2,4] [1,2,3,4] `shouldBe` (3,0)
      check [1,2,2,4] [2,1,3,4] `shouldBe` (1,2)
      check [1,2,2,2] [2,2,3,3] `shouldBe` (1,1)
      check [1,2,3,4] [5,6,7,8] `shouldBe` (0,0)
      check [1,2,3,4] [4,3,2,1] `shouldBe` (0,4)
  describe "possible" $ do
    it "having no constraints returns the entire universe" $ do
      possible [] `shouldBe` universe
    it "works for some examples" $ do
      possible' "1112 00 3334 11 5666 00 7888 11 3487 21" `shouldBe` [[3,7,8,3]]

