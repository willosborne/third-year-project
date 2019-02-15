module Utils where

import Slide
import Animation
import Content

makeBullets :: [String]
            -> (Double, Double)
            -> Float
            -> Maybe Float
            -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
makeBullets paras lineGap Nothing = do
  return []
  
         
