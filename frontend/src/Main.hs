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

  -- canvas <- uncheckedCastTo HTMLCanvasElement <$> createElement doc "canvas"
  -- appendChild_ body canvas
  -- canvas <- unsafeToCanvas <$> createElement doc "canvas"
  -- canvas <- castToHTMLCanvasElement $ createElement doc "canvas"
  -- ctx <- liftIO $ getContext canvas
  -- c <- fmap CanvasRenderingContext2D $ liftIO $ getContext canvas "2d"
  w <- getInnerWidth win
  h <- getInnerHeight win
  -- ctx <- canvasContext doc "style=\"border: 1px solid black;\
  --                          \ top:0px; bottom: 0px; left: 0px; right: 0px;\"" 
  ctx <- fixedSizeCanvas doc w h
  -- ctx <- fullScreenCanvas doc 

  -- let con = (Combine (Line 10 10 300 100) (Translate (Rotate (Rect 203 101) 45) 100 100))
  -- render ctx con
  let cross = FillColor 255 0 0 1 $ FPolygon [(0, 0), (50, 0), (50, 50), (100, 50), (100, 100), (50, 100), (50, 200), (0, 200), (0, 100), (-50, 100), (-50, 100), (-50, 50), (0, 50)]
  render ctx $ Translate 200 400 $ Rotate (degreesToRadians 180) $ cross
  render ctx $ Translate 100 100 $ Rotate 90 $ RegularPolygon 5 100
  render ctx $ Translate 500 100 $ Rotate 45 $ Rect 50 100
  render ctx $ Translate 100 300 $ Text "32px Garamond"
                                    (Just 50)
                                    "The FitnessGram Pacer Test is a multi-stage aerobic capacity test designed to..." 

  syncPoint
