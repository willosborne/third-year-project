module Lib
    ( someFunc
    ) where

import Control.Concurrent
import JavaScript.Web.Canvas
import System.Random

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"

  let go = do
        x1 <- randomIO :: IO Double
        y1 <- randomIO :: IO Double
        x2 <- randomIO :: IO Double 
        y2 <- randomIO :: IO Double

        moveTo (x1 * 500) (y1 * 500)
        lineTo (x2 * 500) (y2 * 500)
        stroke
        threadDelay 2000

        go

  go
  
