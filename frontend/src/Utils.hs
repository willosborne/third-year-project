module Utils where

import           Slide
import           Animation
import           Content
import           Render

import           Control.Monad.Writer hiding (fix)

import           Data.Unique

import           Unsafe.Coerce (unsafeCoerce)

import           GHCJS.DOM
import           GHCJS.DOM.Document (Document, getBody)
import qualified GHCJS.DOM.Document as Doc (setTitle)
import           GHCJS.DOM.Types hiding (Animation, Text)
import           GHCJS.DOM.Element
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
import qualified GHCJS.DOM.HTMLCanvasElement as H (getContext) 


makeBulletsAnimated :: Font
                    -> (Float, Float)
                    -> Float
                    -> Maybe Float
                    -> CanvasRenderingContext2D
                    -> [String]
                    -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
makeBulletsAnimated font (x0, y0) lineGap Nothing _ paragraphs  = do
  let loop :: Float -> [String] -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
      loop _ [] = return []
      loop y (para:paras) = do
        if (para == "<spacer>") then do
          list <- loop (y + lineGap) paras
          return list
        else do
          anim <- liftIO $ makeAnimationTagged [ fix (pairI Translate) (PairI (x0, y)) ] $ Text font Nothing para
          tell [anim]
          anims <- loop (y + lineGap) paras
          return $ anim : anims
  list <- loop y0 paragraphs
  return list
makeBulletsAnimated font (x0, y0) lineGap (Just maxWidth) ctx paragraphs  = do
  let loop :: Float -> [String] -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
      loop _ [] = return []
      loop y (para:paras) = do
        if (para == "<spacer>") then do
          list <- loop (y + lineGap) paras
          return list
        else do
          anim <- liftIO $ makeAnimationTagged [ fix (pairI Translate) (PairI (x0, y)) ] $ Text font (Just maxWidth) para
          tell [anim]
          noLines <- liftIO $ wordsToLineCount ctx maxWidth (words para)
          anims <- loop (y + (lineGap * (fromIntegral noLines))) paras
          return $ anim : anims
  save ctx
  setFont ctx font
  list <- loop y0 paragraphs
  restore ctx
  return list


-- TODO: make this return a list of animations rather than just the one - so that points can be animated afterwards
makeBullets :: Font
            -> (Float, Float)
            -> Float
            -> Maybe Float
            -> CanvasRenderingContext2D
            -> [String]
            -> AnimWriter
makeBullets font (x0, y0) lineGap Nothing _ paragraphs  = do
  let loop :: Float -> [String] -> Content
      loop _ [] = Empty
      loop y (para:paras) =
        if (para == "<spacer>") then
          (loop (y + lineGap) paras)
        else let content = Translate 0 y $ Text font Nothing para
                 c = loop (y + lineGap) paras
             in content <> c
  let content = loop 0 paragraphs
  animation [ fix (pairI Translate) (PairI (x0, y0)) ] content

makeBullets font (x0, y0) lineGap (Just maxWidth) ctx paragraphs  = do
  let loop :: Float -> [String] -> IO Content
      loop _ [] = return Empty
      loop y (para:paras) = do
        if (para == "<spacer>")
          then do
            (loop (y + lineGap) paras)
          else do
            let content = Translate 0 y $ Text font (Just maxWidth) para

            noLines <- liftIO $ wordsToLineCount ctx maxWidth (words para)
            c <- loop (y + (lineGap * (fromIntegral noLines))) paras
            return $ content <> c
  save ctx
  setFont ctx font
  content <- liftIO $ loop 0 paragraphs
  restore ctx
  animation [ fix (pairI Translate) (PairI (x0, y0)) ] $ content 

-- makeBulletsStatic font (x0, y0) lineGap (Just maxWidth) ctx paragraphs  = do
--   let loop :: Float -> [String] -> WriterT [(Animation, Unique)] IO [(Animation, Unique)]
--       loop _ [] = return []
--       loop y (para:paras) = do
--         if (para == "<spacer>") then do
--           list <- loop (y + lineGap) paras
--           return list
--         else do
--           anim <- liftIO $ makeAnimationTagged [ fix (pairI Translate) (PairI (x0, y)) ] $ Text font (Just maxWidth) para
--           tell [anim]
--           noLines <- liftIO $ wordsToLineCount ctx maxWidth (words para)
--           anims <- loop (y + (lineGap * (fromIntegral noLines))) paras
--           return $ anim : anims
--   setFont ctx font
--   list <- loop y0 paragraphs
--   return list

-- makeText :: Font -> (Float, Float)

  
delayAnim :: AnimWriter
delayAnim = do
  animation [] Empty

empty :: AnimWriter
empty = do
  return undefined 
  -- return (Animation [] Empty, newUnique)

-- from shine
toContext :: Element -> IO CanvasRenderingContext2D
toContext c = do 
  Just ctx <- H.getContext (unsafeCoerce c) "2d" ["2d"] -- NOTE: extremely hacky way of passing in Element as a JSVal
  return $ unsafeCoerce ctx 

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

setTitle :: String -> IO ()
setTitle title = do
  doc <- currentDocumentUnchecked
  Doc.setTitle doc title

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
