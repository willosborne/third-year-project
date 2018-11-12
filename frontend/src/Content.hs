module Content where

type Font = String -- just represents the javascript name of the font

-- Content - represents on-screen elements.
-- None of these contain position or rotation, as all of those are handled by translations
-- TODO add more shapes
data Content =
  Empty
  | Rect Double Double -- width, height TODO consider filled shapes
  | FRect Double Double -- width, height TODO consider filled shapes
  | Circle Double -- radius
  | FCircle Double -- radius
  | Line Double Double Double Double -- begin, end
  | Path [(Double, Double)] -- list of points to connect with path
  | Polygon [(Double, Double)] -- list of points to connect; join up at end
  | FPolygon [(Double, Double)] -- list of points to connect; join up at end
  | RegularPolygon Int Double -- num. sides, radius
  | FRegularPolygon Int Double -- num. sides, radius
  | Translate Double Double Content --  x, y, content to transform
  | Rotate Double Content -- angle, content 
  | Scale Double Double Content -- xscale, yscale, content
  | Combine Content Content -- combine two Contents   NOTE: consider renaming
  | Text Font (Maybe Float) String  -- font, max width, text  TODO: wrap text rather than squashing it!
  | FillColor Int Int Int Float Content -- fill color
  | StrokeColor Int Int Int Float Content -- fill color
  | StrokeWidth Float Content
  -- TODO add splines
