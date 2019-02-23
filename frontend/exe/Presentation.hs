module Presentation where


import Slide
import Animation
import Content
import Utils
import GHCJSTime
import ImagePreloader

import Life

-- import Paths_frontend


runPresentation :: IO ()
runPresentation = do
  doc <- getDocument
  ctx <- getContext
  timeRef <- initTime

  -- getDataFileName "frp-event.png" >>= putStrLn
  
  em <- emptyDB
  _ <- updateTime timeRef
  imageDB <- loadImages em [("behaviour", "images/frp-behaviour.png"),
                            ("event", "images/frp-event.png"),
                            ("yoda", "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png")]
  updateTime timeRef >>= (\t -> putStrLn ("Images loaded. Time: " ++ (show t) ++ "ms."))

  slideshow ctx doc imageDB 60 $ do
    slide $ do
      animation [ fix (pairI Translate) (PairI (200, 300)) ] $ Text "40px Garamond" Nothing "Genzai: Declarative Slideshows using FRP"
    slideGeneric lifeSlide

  
