
module Main where

-- import GHCJS
import GHCJS.DOM
import GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import GHCJS.DOM.Node
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.HTMLHyperlinkElementUtils
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
-- import GHCJS.DOM.CanvasPath
import GHCJS.DOM.NonElementParentNode (getElementById)
-- import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM.HTMLImageElement

import Data.Monoid ((<>))

import Unsafe.Coerce (unsafeCoerce)
import qualified JavaScript.Web.Canvas

-- import Data.JSString.Internal.Type (JSString)

import Content
import Render
import Image
import ImagePreloader

import Control.Monad.Reader
import Control.Concurrent

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
drawing = Translate 400 400 $
  (FillColor (RGB 255 0 0) $ StrokeWidth 3 $ Rotate 180 $ Scale 3 3 cross) <> -- filled, stroked path
  (FillColor (RGB 0 255 0) $ FRegularPolygon 8 100) <> -- filled, stroked regular polygon
  (StrokeColor (RGB 0 0 255) $ StrokeWidth 4 $ Path [(-50, -50), (50, -50), (-50, 50)]) <> -- coloured path
  (Translate 0 100 $ Text "22px Garamond" (Just 600) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat laoreet vestibulum. Nam pellentesque libero a urna bibendum, at pellentesque libero tincidunt. Mauris elementum neque et lacus sollicitudin blandit. Mauris ut felis sodales, viverra dui vitae, bibendum est. Sed iaculis mauris eget orci maximus rutrum ac quis urna. Aenean fermentum semper sapien vel aliquam. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras consectetur at eros eget tempor. Ut et ligula suscipit, cursus metus et, rutrum tortor. Praesent iaculis efficitur arcu, vitae venenatis neque convallis ac. Nulla at risus purus. Vestibulum sit amet enim condimentum, facilisis nunc ac, iaculis nunc.") <>
  (FillColor (RGBA 0 255 255 0.5) $ Translate 0 150 $ FCircle 50) <>
  (Image "yoda" Original) 

helloMain :: IO ()
helloMain = do
  win <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc

  _ <- on doc click $ do
    (x, y) <- mouseClientXY
    newPara <- uncheckedCastTo HTMLParagraphElement <$> createElement doc "p"
    text <- createTextNode doc $ "Click " ++ show (x, y)
    appendChild_ newPara text
    appendChild_ body newPara

  w <- getInnerWidth win
  h <- getInnerHeight win
  ctx <- fixedSizeCanvas doc w h

  ctx' <- getCtx
  
  -- TODO add quick function to create filled map in one go
  em <- emptyDB
  imageDB <- loadImages em [("yoda", "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png")]
  

  -- runReaderT (render ctx drawing) imageDB
  runReaderT (render ctx (Image "yoda" Original)) imageDB

  -- (ImageData im) <- makeImageData  "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png"
  -- -- flip runReaderT imageDB $ 
  -- drawImage ctx im 100 100

  img <- js_newImage
  -- setCrossOrigin img (Just "anonymous")
  -- setSrc img "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png"
  setSrc img "egg.jpg"
  _ <- on img load $ do
    liftIO $ do
      putStrLn "drawing egg"
      drawImage ctx img 0 0
      putStrLn "egg drawn"
      return ()

  syncPoint

