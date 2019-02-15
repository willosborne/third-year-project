module Main where

import GHCJS.DOM
import GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.NonElementParentNode (getElementById)

import Data.Monoid ((<>))

import Unsafe.Coerce (unsafeCoerce)
import qualified JavaScript.Web.Canvas


import Content
import Image
import ImagePreloader
import Animation
import GHCJSTime
import Slide
import Utils

import Life

run :: a -> a
run = id

main :: IO ()
main = run helloMain

-- from shine
toContext :: Element -> IO CanvasRenderingContext2D
toContext c = do 
  Just ctx <- getContext (unsafeCoerce c) "2d" ["2d"] -- NOTE: extremely hacky way of passing in Element as a JSVal
  return $ unsafeCoerce ctx 

-- canvasStyle :: String
-- canvasStyle = "\"border:1px \
--               \solid #00000; \
--               \top:0px;bottom:0px;left:0px;right:0px;\""

canvasContext :: Document -> String -> IO CanvasRenderingContext2D
canvasContext doc attributes = do
  Just body <- getBody doc
  setInnerHTML body ("<canvas id=\"canvas\" " ++ attributes ++ " </canvas> ")
  Just canvas <- getElementById doc "canvas"
  -- canvas <- elementSetClientWidth w
  -- canvas <- htmlCanvasElementSetHeight h
  toContext canvas

-- | Create a fixed size canvas given the dimensions
fixedSizeCanvas :: Document -> Int -> Int -> IO CanvasRenderingContext2D
fixedSizeCanvas doc x y = canvasContext doc $ attributes x y
  where attributes :: Int -> Int -> String
        attributes x' y' = "width=\""++ show x' ++ "\" \
                           \height=\""++ show y' ++ "\" \
                           \style=\"border:1px \
                           \solid #000000;\
                           \padding: 0px 0px 0px 0px;\""

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO JavaScript.Web.Canvas.Context
--  -- | Create a full screen canvas
-- fullScreenCanvas :: Document -> IO CanvasRenderingContext2D
-- fullScreenCanvas doc = canvasContext doc attributes
--   where attributes :: String
--         attributes = "style=\"border:1px \
--                      \solid #000000; \
--                      \top:0px;bottom:0px;left:0px;right:0px;\""                          

cross :: Content
cross = Translate (-75) (-100) $ FPolygon [(0, 0), (50, 0), (50, 50), (100, 50), (100, 100), (50, 100), (50, 200), (0, 200), (0, 100), (-50, 100), (-50, 100), (-50, 50), (0, 50)]

drawing :: Content
drawing = Translate 200 200 $
  (Image "egg" Original) <>
  (FillColor (RGB 255 0 0) $ StrokeWidth 3 $ Rotate 180 $ Scale 3 3 cross) <> -- filled, stroked path
  (FillColor (RGB 0 255 0) $ FRegularPolygon 8 100) <> -- filled, stroked regular polygon
  (StrokeColor (RGB 0 0 255) $ StrokeWidth 4 $ Path [(-50, -50), (50, -50), (-50, 50)]) <> -- coloured path
  (Translate 0 100 $ Text "22px Garamond" (Just 600) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat laoreet vestibulum. Nam pellentesque libero a urna bibendum, at pellentesque libero tincidunt. Mauris elementum neque et lacus sollicitudin blandit. Mauris ut felis sodales, viverra dui vitae, bibendum est. Sed iaculis mauris eget orci maximus rutrum ac quis urna. Aenean fermentum semper sapien vel aliquam. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras consectetur at eros eget tempor. Ut et ligula suscipit, cursus metus et, rutrum tortor. Praesent iaculis efficitur arcu, vitae venenatis neque convallis ac. Nulla at risus purus. Vestibulum sit amet enim condimentum, facilisis nunc ac, iaculis nunc.") <>
  (FillColor (RGBA 0 255 255 0.5) $ Translate 0 150 $ FCircle 50) <>
  (Translate 300 0 $ Image "yoda" Original) 


testTranslate :: Double -> Content -> Content
testTranslate x c = Translate x 0 c

ball :: Content
ball = (FillColor (RGB 255 0 0) $ StrokeWidth 2 $ FCircle 100)
    <> (Translate 0 100 $ Image "egg" Original)

      
helloMain :: IO ()
helloMain = do
  win <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  -- body <- getBodyUnchecked doc
  w <- getInnerWidth win
  h <- getInnerHeight win
  ctx <- fixedSizeCanvas doc w h
  timeRef <- initTime
  
  -- TODO add quick function to create filled map in one go
  -- maybe >> into flip
  em <- emptyDB
  putStrLn "Stalling till images have loaded..."
  _ <- updateTime timeRef
  imageDB <- loadImages em [("egg", "egg.jpg"), ("yoda", "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png")]
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
      a <- animation [ fix (pairI Translate) (PairI (100, 100)) ] $ Text "22px Garamond" (Just 600) "Sample text, please ignore"
      _ <- animation [ fix (pairI Translate) (PairI (100, 130)) ] $ Text "22px Garamond" (Just 600) "More sample text, please ignore"
      chain [ chainTween (tween (pairI Translate)) (PairI (100, 400)) easeSin 2000000 ] a
    slideGeneric lifeSlide
    slide $ do
      c <- animation [ makeTween tweenTranslate (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
                          , fix (colorI FillColor) (ColorI (RGB 0 0 255)) ] $ FCircle 100
      r <- animation [ makeTween tweenFillColor (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 1000000] $ FRect 100 100
      _ <- chain [ chainTween tweenTranslate (PairI (400, 300)) easeSin 2000000] c
      chain [ chainTween tweenTranslate (PairI (800, 600)) easeSin 2000000] r
    slide $ do
      animation [ makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 100)) easeSin 2000000 ] $ FRect 100 100
