module Animation where

import Event
import Content

-- i'm gonna hack out something very simple
  
type AnimControl = Double


-- easing function
lerp :: Double -> Double -> AnimControl -> Double
lerp v0 v1 t = v0 + (v1 - v0) * t

easeLinear :: AnimControl -> AnimControl
easeLinear = id

easeSin :: AnimControl -> AnimControl
easeSin t = sin (t * pi/2)

transition :: (Double -> Content -> Content) -> Double -> Double -> (AnimControl -> AnimControl) -> AnimControl -> Content -> Content
transition modifier v0 v1 ease t c = modifier v c
  where
    v = lerp v0 v1 $ ease t


