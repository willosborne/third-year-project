module Slide where

import Content
import Image
import Render
import Animation

data Stage = Stage

type SlideM = StateT (Int, AnimControl) ReaderT [Stage] IO ()


-- renderSlide :: SlideM -> IO ()
-- renderSlide 
