module Presentation where


import Slide
import Animation
import Content
import Utils
import GHCJSTime
import ImagePreloader
import Eases
import Image

import Data.Monoid ((<>))

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
  imageDB <- loadImages em [("behaviour", "images/frp-behavior.png"),
                            ("event", "images/frp-event.png")]
  updateTime timeRef >>= (\t -> putStrLn ("Images loaded. Time: " ++ (show t) ++ "ms."))
  
  let font = "22px Garamond"
  w <- slideWidth
  h <- slideHeight

  slideshow ctx doc imageDB 60 $ do
    slide $ do
      animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ Text "40px Garamond" Nothing "Genzai: Declarative Slideshows using FRP"
      
    -- slide $ do
    --   [_, _, a, _] <- makeBullets "22px Garamond" (100, 100) 40 (Just 600) ctx [
    --     "This is the first line of text. It's not very long at all.",
    --     "This is the second line of text. It's still not that long, but it's a bit longer than the first.",
    --     "This is the third and final line of text. By comparison to the other two lines, it's exceptionally long, potentially rivalling the likes of Tolstoy's War and Peace and the great literary classics of the 19th Century.",
    --     "This line is pretty short." ]
    --   chain [ chainTween tweenTranslate (PairI (200, 200)) easeOutElastic 1000000 ] a
    --   animation [ makeTween tweenTranslate (PairI (400, -300)) (PairI (400, 400)) easeOutBounce 1100000] $ Scale 0.7 0.7 $ Image "yoda" Original

    slide $ do
      let slEvent = Translate (w %% 25) 200 $
                    (AlignText AlignCenter $ Text font (Just 400) "Event")
                 <> (Translate 0 100 $ Image "event" Original)
          slBehaviour = Translate (w %% 75) 200 $
                        (AlignText AlignCenter $ Text font (Just 400) "Behaviour")
                     <> (Translate 0 100 $ Image "behaviour" Original)
               
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text "30px Garamond" Nothing "FRP: Events and Behaviours"
      animation [] slEvent
      animation [] slBehaviour
    slideGeneric lifeSlide

  
