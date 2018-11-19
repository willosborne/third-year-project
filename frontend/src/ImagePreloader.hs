{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module ImagePreloader where

import Data.Map.Strict as Map hiding (map, foldl)
import Data.JSString.Internal.Type (JSString(..))

import GHCJS.DOM.HTMLImageElement

import Image
-- import Paths_frontend

type ImageDB = (Map String ImageData, ImageData)

emptyDB :: IO ImageDB
emptyDB = do
  missing <- imageMissing
  return (empty, missing)

imageMissing :: IO ImageData
imageMissing = do
  -- src <- getDataFileName "missing.jpg"
  -- putStrLn src
  -- makeImageData src
  makeImageData "https://www.nationalpetregister.org/assets/img/no-photo.jpg"
  
getImage :: ImageDB -> String -> IO ImageData
getImage (imageMap, missing) imageName = do
  putStrLn imageName
  return $ findWithDefault missing imageName imageMap

foreign import javascript interruptible "preloadImage($1, $c);"
  js_preloadImage :: JSString -> IO HTMLImageElement

foreign import javascript interruptible "preloadImages($1, $c);"
  js_preloadImages :: [JSString] -> IO HTMLImageElement

-- foreign import javascript unsafe "$1.onload = $2"
--   js_imageOnLoad :: HTMLImageElement -> T.JSFun a -> IO ()
-- -- https://github.com/ghcjs/ghcjs/wiki/A-few-examples-of-Foreign-Function-Interface

-- TODO refactor this to have some kind of error checking for invalid files
-- Has to wait for onload for every image before it can proceed
loadImages :: ImageDB -> [(String, FilePath)] -> IO ImageDB
loadImages (imageMap, missing) pairs = do
  let (names, urls) = unzip pairs
  images <- js_preloadImages $ map toJSString urls
  
  return $ (foldl ins imageMap (zip names images), missing)
  where
    ins t (k, a) = insert k a t
-- loadImages imageDB [] = return imageDB
-- loadImages (imageMap, missing) ((name, url) : toLoad) = do
--   putStrLn $ name ++ ": " ++ url
--   img <- makeImageData url
--   loadImages ((insert name img imageMap), missing) toLoad

{-
NOTE:
Need to design system to wait for the onload callback for every image before proceeding
Could do directly inside FFI js code, but that's kinda grim tbh
Also want to load all at once, not one at a time!
-}
