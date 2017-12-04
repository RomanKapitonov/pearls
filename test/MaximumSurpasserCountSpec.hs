module MaximumSurpasserCountSpec (main, spec) where

import Test.Hspec
import MaximumSurpasserCount (
        msc1
      , msc2
      , msc3)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "msc1" $ do
    it "finds msc in quadratic time" $ do
      msc1 testData `shouldBe` 6
  describe "msc2" $ do
    it "finds msc in quadratic time" $ do
      msc2 testData `shouldBe` 6
  describe "msc2" $ do
    it "finds msc in nlog(n) time" $ do
      msc2 testData `shouldBe` 6

testData :: [Char]
testData = ['t', 'e', 'l', 'g', 'r', 'a', 'p', 'h', 'e', 'r']