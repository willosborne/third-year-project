module ImagePreloaderSpec where

import Test.Hspec

import Image
import ImagePreloader

import Data.Map.Strict as Map hiding (map, foldl)

spec :: Spec
spec = do
  describe "ImagePreloader" $ before imageMissing $ do
    -- it "Returns correct number of elements" $ do
    --   em <- emptyDB
    --   (db, missing) <- loadImages em [("thanos", "https://www.businessinsider.in/thumb/msid-69054170,width-600,resizemode-4/Google-let-users-play-with-Thanoss-destructive-power.jpg?112225")]
    --   (length . keys) db `shouldBe` 1 :: Int
      
    it "Creates empty image db" $ \m -> do
      -- em <- emptyDB
      -- em `shouldBe` (empty, missing) :: ImageDB 
      emptyDB `shouldReturn` (empty, m)
