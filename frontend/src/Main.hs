module Main where

-- import GHCJS
import GHCJS.DOM
import GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
-- import GHCJS.DOM.Node
-- import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
-- import GHCJS.DOM.EventM
-- import GHCJS.DOM.GlobalEventHandlers
-- import GHCJS.DOM.HTMLHyperlinkElementUtils
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
-- import GHCJS.DOM.CanvasPath
import GHCJS.DOM.NonElementParentNode (getElementById)
-- import Control.Monad.IO.Class (MonadIO(..))

import Data.Monoid ((<>))

import Unsafe.Coerce (unsafeCoerce)
-- import JavaScript.Web.Canvas

import Content
import Render

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
drawing = Translate 400 400 $
  (FillColor (RGB 255 0 0) $ StrokeWidth 3 $ Rotate 180 $ Scale 3 3 cross) <> -- filled, stroked path
  (FillColor (RGB 0 255 0) $ FRegularPolygon 8 100) <> -- filled, stroked regular polygon
  (StrokeColor (RGB 0 0 255) $ StrokeWidth 4 $ Path [(-50, -50), (50, -50), (-50, 50)]) <> -- coloured path
  (Translate 0 100 $ Text "22px Garamond" (Just 600) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat laoreet vestibulum. Nam pellentesque libero a urna bibendum, at pellentesque libero tincidunt. Mauris elementum neque et lacus sollicitudin blandit. Mauris ut felis sodales, viverra dui vitae, bibendum est. Sed iaculis mauris eget orci maximus rutrum ac quis urna. Aenean fermentum semper sapien vel aliquam. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras consectetur at eros eget tempor. Ut et ligula suscipit, cursus metus et, rutrum tortor. Praesent iaculis efficitur arcu, vitae venenatis neque convallis ac. Nulla at risus purus. Vestibulum sit amet enim condimentum, facilisis nunc ac, iaculis nunc.") <>
  (FillColor (RGBA 0 255 255 0.5) $ Translate 0 150 $ FCircle 50)

helloMain :: IO ()
helloMain = do
  win <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  -- body <- getBodyUnchecked doc

  -- _ <- on doc click $ do
  --   (x, y) <- mouseClientXY
  --   elem <- createElement doc "p"
  --   temp <- setAttribute elem "style" "text-color:green;"
  --   newPara <- uncheckedCastTo HTMLParagraphElement temp
  --   text <- createTextNode doc $ "Click " ++ show (x, y)
  --   appendChild_ newPara text
  --   appendChild_ body newPara

  w <- getInnerWidth win
  h <- getInnerHeight win
  ctx <- fixedSizeCanvas doc w h
  -- let crosses = (FillColor 255 0 0 1 cross) <> (Rotate 180 (FillColor 0 0 255 1 cross)) <> (Rotate 90 (FillColor 0 255 0 1 cross))
  -- let r = FRect 100 80
  -- let rects = (FillColor 0 0 255 1 (StrokeWidth 5 (FCircle 100))) <> (FillColor 255 0 0 1 r) <> (Translate 30 30 (FillColor 0 255 0 1 r))
  -- render ctx $ Translate 300 300 rects
  -- render ctx $ Translate 100 100 $ Rotate 90 $ RegularPolygon 5 100
  -- render ctx $ Translate 500 100 $ Rotate 45 $ Rect 50 100
  -- render ctx $ Translate 100 300 $ FillColor 255 0 0 1 $ Text "32px Garamond"
  --                                                         (Just 200)
  --                                                         "The FitnessGram Pacer Test is a multi-stage aerobic capacity test designed to..." 

  render ctx drawing

  syncPoint
