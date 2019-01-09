module GHCJSTime where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef

-- based on https://github.com/ivanperez-keera/haskanoid/blob/ghcjs/src/GHCJSNow.hs

initTime :: IO (IORef Int)
initTime = do
  ref <- newIORef 0
  m <- toMillisecs <$> getPOSIXTime
  writeIORef ref m
  return ref

getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

updateTime :: IORef Int -> IO Int
updateTime ref = do
  newTime <- toMillisecs <$> getPOSIXTime
  oldTime <- readIORef ref
  writeIORef ref newTime
  return $ newTime - oldTime

printTime :: IO ()
printTime = do
  posixTime <- getPOSIXTime
  putStrLn $ show posixTime

toMillisecs :: RealFrac a => a -> Int
toMillisecs m = round (m * 1000)

toSecs :: Int -> Double
toSecs s = fromIntegral s / 100
