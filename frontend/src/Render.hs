module Render where

import Content

import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.CanvasPath


degreesToRadians degrees = degrees * (pi / 180)

render :: CanvasRenderingContext2D -> Content -> IO ()
render ctx (Line x1 y1 x2 y2) = do
  moveTo ctx x1 y1
  lineTo ctx x2 y2
  stroke ctx
render ctx (Rect w h) = do
  rect ctx (-w/2) (-h/2) w h
  stroke ctx

render ctx (Combine c1 c2) = do
  render ctx c1
  render ctx c2

render ctx (Translate c x' y') = do
  let x = realToFrac x'
  let y = realToFrac y'
  translate ctx x y
  render ctx c
  translate ctx (-x) (-y)

render ctx (Scale c x' y') = do
  scale ctx x y
  render ctx c
  scale ctx (-x) (-y)
  where x = realToFrac x'
        y = realToFrac y'

render ctx (Rotate c angle') = do
  rotate ctx angle
  render ctx c
  rotate ctx (-angle)
  where angle = realToFrac angle'

render ctx (Path []) = return ()
render ctx (Path ((x, y) : points)) = do
  moveTo ctx x y
  renderPath ctx points
  stroke ctx

  -- TODO: can almost certainly do this neatly with map
  where renderPath ctx [] = return ()
        renderPath ctx ((px, py) : ps) = do
          lineTo ctx px py
          renderPath ctx ps

-- NOTE: do i need to move back after rendering a line
render ctx (RegularPolygon sides radius) = do
  let step = (pi * 2) / (fromIntegral sides)
      max = sides - 1
  moveTo ctx radius 0
  poly ctx 0 step
  stroke ctx

  where
    poly ctx n step
      | n < (step - 1) = do
            let x' = radius * (sin (step * n))
            let y' = radius * (cos (step * n))
            lineTo ctx x' y'
      | otherwise = closePath ctx

render ctx (Circle radius) = do
  render ctx (RegularPolygon 60 radius)

render _ _ = undefined
