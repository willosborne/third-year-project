module Render where

import Content

import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.CanvasPath
import GHCJS.DOM.Enums (CanvasWindingRule (CanvasWindingRuleNonzero))
import GHCJS.DOM.Types (CanvasStyle (..))

import Data.List (intercalate)
import GHCJS.Prim (toJSString)

import qualified GHCJS.DOM.TextMetrics as TextMetrics (getWidth) 


-- import Data.List.Split (splitOn)

degreesToRadians :: Floating a => a -> a
degreesToRadians degrees = degrees * (pi / 180)

render :: CanvasRenderingContext2D -> Content -> IO ()
render ctx (Line x1 y1 x2 y2) = do
  moveTo ctx x1 y1
  lineTo ctx x2 y2
  stroke ctx

render ctx (Rect w h) = do
  rect ctx (-w/2) (-h/2) w h
  stroke ctx

render ctx (FRect w' h') = do
  -- rect ctx (-w/2) (-h/2) w h
  fillRect ctx (-w/2) (-h/2) w h
  stroke ctx
  where
    w = realToFrac w'
    h = realToFrac h'

render ctx (Combine c1 c2) = do
  render ctx c1
  render ctx c2

render ctx (Translate x' y' c) = do
  let x = realToFrac x'
  let y = realToFrac y'
  translate ctx x y
  render ctx c
  translate ctx (-x) (-y)

render ctx (Scale x' y' c) = do
  scale ctx x y
  render ctx c
  scale ctx (-x) (-y)
  where x = realToFrac x'
        y = realToFrac y'

render ctx (Rotate angle' c) = do
  rotate ctx angle
  render ctx c
  rotate ctx (-angle)
  where angle = realToFrac angle'

render ctx (FillColor r g b a c) = do
  let str = "rgba("
         ++ intercalate "," [show r, show g, show b, show a]
         ++ ")"
  let color = toJSString str
  setFillStyle ctx $ CanvasStyle color

  render ctx c

  setFillStyle ctx $ CanvasStyle $ toJSString "#000000"

render ctx (StrokeColor r g b a c) = do
  let str = "rgba("
         ++ intercalate "," [show r, show g, show b, show a]
         ++ ")"
  let color = toJSString str
  setStrokeStyle ctx $ CanvasStyle color

  render ctx c

  setStrokeStyle ctx $ CanvasStyle $ toJSString "#000000"

render _ (Path []) = return ()
render ctx (Path ((x, y) : points)) = do
  moveTo ctx x y
  renderPath points
  stroke ctx

  -- TODO: can almost certainly do this neatly with mapM
  where renderPath [] = return ()
        renderPath ((px, py) : ps) = do
          lineTo ctx px py
          renderPath ps

render _ (Polygon []) = return ()
render ctx (Polygon ((x, y) : points)) = do
  moveTo ctx x y
  renderPath points
  closePath ctx
  stroke ctx

  -- TODO: can almost certainly do this neatly with mapM
  where renderPath [] = return ()
        renderPath ((px, py) : ps) = do
          lineTo ctx px py
          renderPath ps

render _ (FPolygon []) = return ()
render ctx (FPolygon ((x, y) : points)) = do
  moveTo ctx x y
  renderPath points
  closePath ctx
  fill ctx $ Just CanvasWindingRuleNonzero
  stroke ctx

  -- TODO: can almost certainly do this neatly with mapM
  where renderPath [] = return ()
        renderPath ((px, py) : ps) = do
          lineTo ctx px py
          renderPath ps

-- NOTE: do i need to move back after rendering a line
render ctx (RegularPolygon sides radius) = do
  let step = (pi * 2) / (fromIntegral sides)
  moveTo ctx radius 0
  poly 0 step
  stroke ctx

  where
    poly n step
      | n < (sides) = do
            let x' = radius * (cos (step * (fromIntegral n)))
            let y' = radius * (sin (step * (fromIntegral n)))
            lineTo ctx x' y'
            poly (n + 1) step
      | otherwise = closePath ctx

render ctx (FRegularPolygon sides radius) = do
  let step = (pi * 2) / (fromIntegral sides)
  moveTo ctx radius 0
  poly 0 step
  fill ctx $ Just CanvasWindingRuleNonzero
  stroke ctx

  where
    poly n step
      | n < (sides) = do
            let x' = radius * (cos (step * (fromIntegral n)))
            let y' = radius * (sin (step * (fromIntegral n)))
            lineTo ctx x' y'
            poly (n + 1) step
      | otherwise = closePath ctx

render ctx (Circle radius) = do
  render ctx (RegularPolygon 60 radius)

render ctx (FCircle radius) = do
  render ctx (FRegularPolygon 60 radius)

render ctx (Text font width' text) = do
  setFont ctx font
  setTextAlign ctx "left"
  case width' of
    Just width -> renderTextWrapped ctx text width 40
    _          -> fillText ctx text 0 0 Nothing
  
render _ _ = undefined


renderTextWrapped :: CanvasRenderingContext2D -> String -> Float -> Float -> IO ()
renderTextWrapped ctx text maxWidth lineHeight = do
  lines' <- ((wordsToLines ctx maxWidth) . words) text
  renderLines 0 lines'
  where
    renderLines :: Float -> [String] -> IO ()
    renderLines _ []           = return ()
    renderLines y (line:lines) = do
      fillText ctx line 0 y Nothing 
      renderLines (y + lineHeight) lines

              
wordsToLines :: CanvasRenderingContext2D -> Float -> [String] -> IO [String]
wordsToLines _ _ [] = return []
wordsToLines ctx maxWidth wordList = wordsToLinesInternal wordList ""
  where
    wordsToLinesInternal :: [String] -> String -> IO [String]
    wordsToLinesInternal [] line           = return [line]
    wordsToLinesInternal (word:words) line = do
      let lineWord = line ++ " " ++ word
      textMet <- measureText ctx lineWord
      lineWordLength <- TextMetrics.getWidth textMet
      case lineWordLength <= maxWidth of
        True        -> wordsToLinesInternal words lineWord
        _           -> (line :) <$> wordsToLinesInternal words word
