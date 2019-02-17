module Utils where

import Slide
import Animation
import Content
import Render

import Control.Monad.Writer hiding (fix)
-- import Control.Monad.Trans

import Data.Unique

import GHCJS.DOM.CanvasRenderingContext2D

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
        return $ anims ++ [anim]
  setFont ctx font
  list <- loop y0 paragraphs
  return list

empty :: AnimWriter
empty = do
  -- animation [] Empty
  animation [] Empty
  
         
