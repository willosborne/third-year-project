module Utils where

import           Slide
import           Animation
import           Content
import           Render

import           Control.Monad.Writer hiding (fix)

import           Data.Unique

import           Unsafe.Coerce (unsafeCoerce)

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Types hiding (Animation, Text)
import           GHCJS.DOM.Element
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import qualified GHCJS.DOM.HTMLCanvasElement as H (getContext) 

makeBullets :: Font
            -> (Float, Float)
            -> Float
            -> Maybe Float
            -> CanvasRenderingContext2D
            -> [String]
            -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
makeBullets font (x0, y0) lineGap Nothing _ paragraphs  = do
  let loop :: Float -> [String] -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
      loop _ [] = return []
      loop y (para:paras) = do
        anim <- liftIO $ makeAnimationTagged [ fix (pairI Translate) (PairI (x0, y)) ] $ Text font Nothing para
        tell [anim]
        anims <- loop (y + lineGap) paras
        return $ anims ++ [anim]
  list <- loop y0 paragraphs
  return list
makeBullets font (x0, y0) lineGap (Just maxWidth) ctx paragraphs  = do
  let loop :: Float -> [String] -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
      loop _ [] = return []
      loop y (para:paras) = do
        anim <- liftIO $ makeAnimationTagged [ fix (pairI Translate) (PairI (x0, y)) ] $ Text font (Just maxWidth) para
        tell [anim]
        noLines <- liftIO $ wordsToLineCount ctx maxWidth (words para)
        anims <- loop (y + (lineGap * (fromIntegral noLines))) paras
        return $ anim : anims
  setFont ctx font
  list <- loop y0 paragraphs
  return list

-- makeText :: Font -> (Float, Float)

  
empty :: AnimWriter
empty = do
  animation [] Empty


-- from shine
toContext :: Element -> IO CanvasRenderingContext2D
toContext c = do 
  Just ctx <- H.getContext (unsafeCoerce c) "2d" ["2d"] -- NOTE: extremely hacky way of passing in Element as a JSVal
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

getDocument :: IO Document
getDocument = currentDocumentUnchecked

getContext :: IO CanvasRenderingContext2D
getContext = do
  win <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  w <- getInnerWidth win
  h <- getInnerHeight win
  fixedSizeCanvas doc w h

-- foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
--   getCtx :: IO JavaScript.Web.Canvas.Context
--  -- | Create a full screen canvas
-- fullScreenCanvas :: Document -> IO CanvasRenderingContext2D
-- fullScreenCanvas doc = canvasContext doc attributes
--   where attributes :: String
--         attributes = "style=\"border:1px \
--                      \solid #000000; \
--                      \top:0px;bottom:0px;left:0px;right:0px;\""                          
slideWidth :: IO Float
slideWidth = do
  win <- currentWindowUnchecked
  fromIntegral <$> getInnerWidth win

slideHeight :: IO Float
slideHeight = do
  win <- currentWindowUnchecked
  fromIntegral <$> getInnerHeight win

-- (%-) :: Float -> IO Float
-- xPercent %- 

(%%) :: Float -> Float -> Float
val %% percent = (percent / 100) * val
