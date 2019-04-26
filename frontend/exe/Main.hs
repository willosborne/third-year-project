module Main where

import GHCJS.DOM
import GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.NonElementParentNode (getElementById)

import Data.Monoid ((<>))

import qualified JavaScript.Web.Canvas


import Content
import Image
import ImagePreloader
import Animation
import GHCJSTime
import Slide
import Utils
import Eases

import Life
import Presentation


main :: IO ()
main = runPresentation
-- main = helloMain


cross :: Content
cross = Translate (-75) (-100) $ FPolygon [(0, 0), (50, 0), (50, 50), (100, 50), (100, 100), (50, 100), (50, 200), (0, 200), (0, 100), (-50, 100), (-50, 100), (-50, 50), (0, 50)]

drawing :: Content
drawing = Translate 200 200 $
  (Image "example" Original) <>
  (FillColor (RGB 255 0 0) $ StrokeWidth 3 $ Rotate 180 $ Scale 3 3 cross) <> -- filled, stroked path
  (FillColor (RGB 0 255 0) $ FRegularPolygon 8 100) <> -- filled, stroked regular polygon
  (StrokeColor (RGB 0 0 255) $ StrokeWidth 4 $ Path [(-50, -50), (50, -50), (-50, 50)]) <> -- coloured path
  (Translate 0 100 $ Text "22px Garamond" (Just 600) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat laoreet vestibulum. Nam pellentesque libero a urna bibendum, at pellentesque libero tincidunt. Mauris elementum neque et lacus sollicitudin blandit. Mauris ut felis sodales, viverra dui vitae, bibendum est. Sed iaculis mauris eget orci maximus rutrum ac quis urna. Aenean fermentum semper sapien vel aliquam. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras consectetur at eros eget tempor. Ut et ligula suscipit, cursus metus et, rutrum tortor. Praesent iaculis efficitur arcu, vitae venenatis neque convallis ac. Nulla at risus purus. Vestibulum sit amet enim condimentum, facilisis nunc ac, iaculis nunc.") <>
  (FillColor (RGBA 0 255 255 0.5) $ Translate 0 150 $ FCircle 50) <>
  (Translate 300 0 $ Scale 0.2 0.2 $ Image "nasa" Original) 


ball :: Content
ball = (FillColor (RGB 255 0 0) $ StrokeWidth 2 $ FCircle 100)
    <> (Translate 0 100 $ Image "egg" Original)

      
helloMain :: IO ()
helloMain = do
  doc <- currentDocumentUnchecked
  ctx <- getContext
  timeRef <- initTime
  
  -- TODO add quick function to create filled map in one go
  -- maybe >> into flip
  em <- emptyDB
  putStrLn "Stalling till images have loaded..."
  _ <- updateTime timeRef
  imageDB <- loadImages em [("example", "http://domaingang.com/wp-content/uploads/2012/02/example.png"), ("yoda", "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png"), ("nasa", "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e5/NASA_logo.svg/1224px-NASA_logo.svg.png")]
  updateTime timeRef >>= (\t -> putStrLn ("Loaded. Time: " ++ (show t) ++ "ms."))
  
  -- animate ctx imageDB $ \t -> return $ Translate (t* 100) 200 $ Scale ((sin t)) ((sin (t * 2)) * 0.5 + 1) $ Image "yoda" Original 

  -- simple runProgram example with (time, speed) as the state
  -- let initialState = (0.0, 1.0, 0.0)
  --     renderState (t, _, angle) = let k = (sin (t * 1.5)) + 1
  --                     in
  --                       return $ Translate 600 300 $ Scale k k $ Rotate angle $ FillColor (RGB 255 0 0) $ Image "yoda" Original
  --     processEvent ev (t, speed, angle) = do
  --       case ev of
  --         EventMouseDown _ _ ButtonLeft -> return (t, speed + 0.1, angle)
  --         EventMouseDown _ _ ButtonRight -> return (t, speed - 0.1, angle)
  --         EventKeyDown ArrowRight -> return (t, speed, angle + 10.0)
  --         EventKeyDown ArrowLeft -> return (t, speed, angle - 10.0)
  --         _ -> return (t, speed, angle)
  --     processTime t (_, speed, angle) = return (t * speed, speed, angle)
  -- runProgram ctx doc imageDB initialState renderState processEvent processTime

  slideshow ctx doc imageDB 60 $ do
    slide $ do
      animation [] drawing
    slide $ do
      [_, a, _, _] <- makeBulletsAnimated "22px Garamond" (100, 100) 40 (Just 600) ctx [
        "This is the first line of text. It's not very long at all.",
        "This is the second line of text. It's still not that long, but it's a bit longer than the first.",
        "This is the third and final line of text. By comparison to the other two lines, it's exceptionally long, potentially rivalling the likes of Tolstoy's War and Peace and the great literary classics of the 19th Century.",
        "This line is pretty short." ]
      chain [ chainTween tweenTranslate (PairI (200, 200)) easeOutElastic 1000000 ] a
      animation [ makeTween tweenTranslate (PairI (400, -300)) (PairI (400, 400)) easeOutBounce 1100000] $ Scale 0.7 0.7 $ Image "example" Original
    slide $ do
      a <- animation [ fix (pairI Translate) (PairI (100, 100)) ] $ Text "22px Garamond" (Just 600) "Sample text, please ignore"
      empty
      _ <- animation [ fix (pairI Translate) (PairI (100, 130)) ] $ Text "22px Garamond" (Just 600) "More sample text, please ignore"
      chain [ chainTween (tween (pairI Translate)) (PairI (100, 400)) easeOutBounce 1000000 ] a
    slideGeneric lifeSlide
    slide $ do
      c <- animation [ makeTween tweenTranslate (PairI (100, 100)) (PairI (600, 500)) easeInOutCubic 2000000
                          , fix (colorI FillColor) (ColorI (RGB 0 0 255)) ] $ FCircle 100
      r <- animation [ makeTween tweenFillColor (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 1000000] $ FRect 100 100
      _ <- chain [ chainTween tweenTranslate (PairI (400, 300)) easeSin 2000000] c
      chain [ chainTween tweenTranslate (PairI (800, 600)) easeSin 2000000] r
    slide $ do
      animation [ makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 100)) easeSin 2000000 ] $ FRect 100 100
