module Utils where

import Slide
import Animation
import Content

import Control.Monad.Writer
import Control.Monad.Trans

import Data.Unique

makeBullets :: [String]
            -> (Float, Float)
            -> Float
            -> Maybe Float
            -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
makeBullets paras topLeft lineGap Nothing = do
  return []
  
         
