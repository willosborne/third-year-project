module Image where

import GHCS.DOM.HTMLImageElement

newtype ImageData = ImageData { unImageData :: HTMLImageElement } deriving Eq


