{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

-- Code taken from https://github.com/fgaz/shine/blob/master/src/Graphics/Shine/Image.hs

module Image where

import JavaScript.Web.Canvas
import GHCJS.Types

import GHCJS.DOM.HTMLImageElement

newtype ImageData = ImageData { unImageData :: HTMLImageElement } deriving Eq

instance Show ImageData where
  show _ = "ImageData"

data ImageSize =
  Original
  | Scaled Float Float
  deriving (Eq, Show)

foreign import javascript unsafe "$r = new Image();"
  js_newImage :: IO HTMLImageElement

-- foreign import javascript unsafe "$1.src = $2;"
--   setSrc' :: Image -> JSString -> IO ()

-- foreign import javascript unsafe "$r = new Image();"
--   newImage' :: IO Image

-- foreign import javascript unsafe "$4.drawImage($1,$2,$3);"
--   drawImage' :: Image -> Double -> Double -> Context -> IO ()

makeImageData :: FilePath -> IO ImageData
makeImageData url = do
  img <- js_newImage
  setSrc img url
  return $ ImageData img
