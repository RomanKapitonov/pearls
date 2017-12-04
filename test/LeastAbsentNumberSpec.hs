module LeastAbsentNumberSpec (main, spec) where

import Test.Hspec
import LeastAbsentNumber (
        minfree1
      , minfree2
      , minfree3
      , minfree4
      , minfree5
      , minfree6)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minfree1" $ do
    it "finds minimal free element" $ do
      (minfree1 testData) `shouldBe` 15
  describe "minfree2" $ do
    it "finds minimal free element" $ do
      (minfree2 testData) `shouldBe` 15
  describe "minfree3" $ do
    it "finds minimal free element" $ do
      (minfree3 testData) `shouldBe` 15
  describe "minfree4" $ do
    it "finds minimal free element" $ do
      (minfree4 testData) `shouldBe` 15
  describe "minfree5" $ do
    it "finds minimal free element" $ do
      (minfree5 testData) `shouldBe` 15
  describe "minfree6" $ do
    it "finds minimal free element" $ do
      (minfree6 testData) `shouldBe` 15

testData :: [Int]
testData = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
