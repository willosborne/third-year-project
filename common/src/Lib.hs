module Lib where

import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Content

someFunc :: IO ()
someFunc = do
  putStrLn "Yeet"


runTests :: IO ()
runTests = hspec $ do
  describe "Content" $ do
    it "Test" $ do
      head [23 ..] `shouldBe` (23 :: Int)
