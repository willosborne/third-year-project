module Spec where

import Test.Hspec

import qualified ImagePreloaderSpec
import qualified AnimationSpec
import qualified RenderSpec

spec :: Spec
spec = do
  describe "ImagePreloader" ImagePreloaderSpec.spec
  describe "Animation" AnimationSpec.spec
  describe "Render" RenderSpec.spec
