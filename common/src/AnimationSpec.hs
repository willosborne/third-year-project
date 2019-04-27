module AnimationSpec where

import Test.Hspec
import Test.QuickCheck
import Animation

spec :: Spec
spec = do
  describe "Interpolation" $ do
    it "Interpolates Floats" $ property $ do
      \t -> interpolate (FloatI 0.0) (FloatI 100.0) t == FloatI (100.0 * (realToFrac t))

    it "Interpolates Ints" $ property $ do
      \t -> interpolate (IntI 0) (IntI 100) t == (IntI $ floor (100.0 * t))

    it "Interpolates Pairs" $ property $ do
      \t -> let t' = realToFrac t
            in interpolate (PairI (0, 0)) (PairI (100, 200)) t == PairI (100.0 * t', 200.0 * t')
