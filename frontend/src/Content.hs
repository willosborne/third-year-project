module Content where

type Font = String -- just represents the javascript name of the font

-- Content - represents on-screen elements.
-- None of these contain position or rotation, as all of those are handled by translations
-- TODO add more shapes
data Content =
  Empty
  | Rect Double Double -- width, height -- TODO consider filled shapes
  | Circle Double -- radius
  | Line Double Double Double Double -- begin, end
  | Path [(Double, Double)] -- list of points to connect with path
  | RegularPolygon Int Double -- num. sides, radius
  | Translate Content Double Double -- content to transform, x, y
  | Rotate Content Double -- content, angle
  | Scale Content Double Double -- content, xscale, yscale
  | Combine Content Content -- combine two Contents
  
