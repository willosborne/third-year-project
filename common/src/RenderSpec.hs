module RenderSpec where

import Test.Hspec
import Render

import Utils


import           GHCJS.DOM.CanvasRenderingContext2D

getContext' :: IO CanvasRenderingContext2D
getContext' = do
  ctx <- getContext
  setFont ctx "24px Garamond"
  return ctx

spec :: Spec
spec = do
  describe "Text wrapping" $ before getContext' $ do
    it "Counts lines correctly" $ do
      \ctx -> wordsToLineCount ctx 250 (words "This should take up about two lines on screen") `shouldReturn` 2
