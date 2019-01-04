{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, FlexibleContexts, QuasiQuotes #-}

module ImagePreloader where

import Data.Map.Strict as Map hiding (map, foldl)
import Data.JSString.Internal.Type (JSString(..))
import qualified Data.JSString as S

import GHCJS.DOM.HTMLImageElement
import GHCJS.DOM.Types (JSVal)
import GHCJS.Marshal.Internal 

import qualified JavaScript.Array

import Image

type ImageDB = (Map String ImageData, ImageData)

emptyDB :: IO ImageDB
emptyDB = do
  missing <- imageMissing
  return (empty, missing)

imageMissing :: IO ImageData
imageMissing = do
  makeImageData "https://www.nationalpetregister.org/assets/img/no-photo.jpg"
  
getImage :: ImageDB -> String -> IO ImageData
getImage (imageMap, missing) imageName = do
  return $ findWithDefault missing imageName imageMap

foreign import javascript interruptible "preloadImage($1, $c);"
  js_preloadImage :: S.JSString -> IO HTMLImageElement

foreign import javascript interruptible "preloadImages($1, $c);"
  js_preloadImages :: JSVal -> IO JavaScript.Array.JSArray


-- TODO refactor this to have some kind of error checking for invalid files
-- Has to wait for onload for every image before it can proceed
loadImages :: ImageDB -> [(String, FilePath)] -> IO ImageDB
loadImages (imageMap, missing) pairs = do
  let (names, urls) = unzip pairs

  arr <- toJSValListOf urls
  jsImages <- js_preloadImages arr
  let images = map (ImageData . HTMLImageElement) $ JavaScript.Array.toList jsImages

  return $ (foldl ins imageMap (zip names images), missing)
  where
    ins t (k, a) = insert k a t

-- https://stackoverflow.com/questions/37861317/how-to-convert-list-to-jsval
-- listToJSVal :: PToJSVal a => [a] -> JSVal
-- listToJSVal xs = foldl push ar xs 
--     where ar = [js'| [] |]
--           push as x = [js'| { `as.push(`x); $r = `as } |]

-- printArray :: JSVal -> IO ()
-- printArray as = [js_| for(i = 0;i < `as.length;i++) { console.log(`as[i]) } |]

{-
NOTE:
Need to design system to wait for the onload callback for every image before proceeding
Could do directly inside FFI js code, but that's kinda grim tbh
Also want to load all at once, not one at a time!
-}
