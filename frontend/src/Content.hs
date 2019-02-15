module Content where

import Data.List (intercalate)

import Image

type Font = String -- just represents the javascript name of the font

-- Content - represents on-screen elements.
-- None of these contain position or rotation, as all of those are handled by translations
data Content =
  Empty
  | Rect Float Float -- width, height TODO consider filled shapes
  | FRect Float Float -- width, height TODO consider filled shapes
  | Circle Float -- radius
  | FCircle Float -- radius
  | Line Float Float Float Float -- begin, end
  | Path [(Float, Float)] -- list of points to connect with path
  | Polygon [(Float, Float)] -- list of points to connect; join up at end
  | FPolygon [(Float, Float)] -- list of points to connect; join up at end
  | RegularPolygon Int Float -- num. sides, radius
  | FRegularPolygon Int Float -- num. sides, radius
  | Translate Float Float Content --  x, y, content to transform
  | Rotate Float Content -- angle, content 
  | Scale Float Float Content -- xscale, yscale, content
  | Combine Content Content -- combine two Contents   NOTE: consider renaming
  | Text Font (Maybe Float) String  -- font, max width, text  TODO: wrap text rather than squashing it!
  | FillColor Color Content -- fill color
  | StrokeColor Color Content -- fill color
  | StrokeWidth Float Content
  | Image String ImageSize
  deriving (Eq, Show)
  -- TODO add splines and arcs

-- used in tweens
data TransformType = Translation | Scaling | Rotation | FillColorChange | StrokeColorChange | StrokeWidthChange | None
                   deriving (Eq, Show)
-- TODO remove None and use a Maybe instead

instance Monoid Content where
  mappend = Combine
  mempty = Empty

-- TODO extend to contain HSVA, etc
data Color = RGBA Int Int Int Float
           | RGB Int Int Int
           deriving (Eq)
-- NOTE: consider adding Transparent as a way of setting no fill

black, white, red, green, blue :: Color
black = (RGB 0 0 0)
white = (RGB 255 255 255)
red = (RGB 255 0 0)
green = (RGB 0 255 0)
blue = (RGB 0 0 255)

instance Show Color where
  show (RGBA r g b a) = "rgba(" ++
    intercalate "," [show r, show g, show b, show a]
    ++ ")"
  show (RGB r g b) = "rgb(" ++
    intercalate "," [show r, show g, show b]
    ++ ")"


