module Lib where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import Test.QuickCheck
import Control.Exception

import Content
import ImagePreloader 

import qualified Spec


someFunc :: IO ()
someFunc = do
  putStrLn "Yeet"


runTests :: IO ()
runTests = hspec Spec.spec
-- runTests = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
