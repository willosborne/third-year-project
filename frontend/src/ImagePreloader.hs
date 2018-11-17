module ImagePreloader where

import Data.Map.Strict as Map

import Image

import Paths_frontend

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
  

foreign import javascript unsafe
""
waitForLoad :: IO ()

-- TODO refactor this to have some kind of error checking for invalid files
-- Has to wait for onload for every image before it can proceed
loadImages :: ImageDB -> [(String, FilePath)] -> IO ImageDB
loadImages imageDB [] = return imageDB
loadImages (imageMap, missing) ((name, url) : toLoad) = do
  putStrLn $ name ++ ": " ++ url
  img <- makeImageData url
  loadImages ((insert name img imageMap), missing) toLoad

{-
NOTE:
Need to design system to wait for the onload callback for every image before proceeding
Could do directly inside FFI js code, but that's kinda grim tbh
Also want to load all at once, not one at a time!
-}
