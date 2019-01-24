module Slide where

import Content
import Image
import Input
import ImagePreloader
import Render
import Animation
import Reactive.FRPSimple
import GHCJSTime

import Web.KeyCode

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.EventM
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.CanvasRenderingContext2D

import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

-- NOTE: consider adding keyPressed event

data Inputs = Inputs {
  clicks :: Event (Int, Int),
  keyPressed :: Event Key,
  next :: Event (),
  previous :: Event ()
  }

generateInputs :: (IsEventTarget document, IsDocument document)
               => document
               -> IO Inputs
generateInputs doc = do
  (click, sendClick) <- newEvent
  (keyPressed, sendKeyPressed) <- newEvent
  (mouseXY, sendMouseXY) <- newEvent
  (next, sendNext) <- newEvent
  (previous, sendPrevious) <- newEvent
  
  bMouseXY <- hold (0, 0) mouseXY
  let clicks = bMouseXY <@ click

  _ <- on doc mouseUp $ do
    (x, y) <- mouseClientXY
    Just button <- toMouseButton <$> mouseButton
    liftIO $ sync $ do
      sendClick ()
      sendNext ()

  _ <- on doc mouseMove $ do
    xy <- mouseClientXY
    liftIO $ sync $ sendMouseXY xy

  _ <- on doc keyDown $ do
    key <- keyCodeLookup . fromIntegral <$> uiKeyCode
    repeat <- uiKeyRepeat
    liftIO $ sync $ when (not repeat) $ do
      sendKeyPressed key
      case key of
        ArrowLeft -> sendPrevious ()
        ArrowRight -> sendNext ()
        Space -> sendNext ()
        _ -> return ()
    

  return Inputs {clicks=clicks, keyPressed=keyPressed, next=next, previous=previous}

-- |Return a tick event that fires based on the FPS, returning the time since last tick each time in milliseconds
generateTick :: Int -> IO (Event Int)
generateTick fps = do
  (tick, sendTick) <- newEvent

  let delay = floor $ (1 / (fromIntegral fps)) * 1000000

  _ <- forkIO $ forever $ do
    threadDelay delay
    sync $ sendTick delay

  return tick


tweenX :: Double -> Double -> AnimControl -> Content -> Content
tweenX x0 x1 t c = Translate x 200 c
  where
    x = lerp x0 x1 t


tweenTranslate :: (Double, Double) -> (Double, Double) -> AnimControl -> Content -> Content
tweenTranslate = tween (pair Translate)

tweenScale :: (Double, Double) -> (Double, Double) -> AnimControl -> Content -> Content
-- tweenTranslate = tweenPair Scale
tweenScale = tween (pair Scale)

-- partially uncurry function to use a pair for interpolation over multiple things
pair :: (a -> a -> b -> b) -> ((a, a) -> b -> b)
pair f = \(x1, x2) b -> f x1 x2 b

-- -- |Generic tween function over two-value properties (i.e. position and scale)
-- tweenPair :: (Double -> Double -> Content -> Content) -- property to tween - e.g. Translate
--           -> (Double, Double) -> (Double, Double) -- start and end
--           -> AnimControl
--           -> Content
--           -> Content
-- tweenPair property p0 p1 t c = property x y c
--   where
--     (x, y) = interpolate p0 p1 t

tweenColor :: (Color -> Content -> Content) -- property to tween - e.g. Translate
          -> Color -> Color -- start and end
          -> AnimControl
          -> Content
          -> Content
tweenColor = tween

tween :: (Interpolate interp)
      => (interp -> Content -> Content)
      -> interp -> interp
      -> AnimControl
      -> Content
      -> Content
tween property v0 v1 t c = property v c
  where
    v = interpolate v0 v1 t

-- NOTE: could interpolate over Double -> Double -> Content -> Content *functions*
-- this gives us a Content -> Content function which can then be applied directly


-- sample an Anims behaviour and render it, all in one go.
renderAnimsB :: CanvasRenderingContext2D -> ImageDB -> Behaviour (Anims) -> IO ()
renderAnimsB ctx imageDB animsB = do
  anims <- sync $ sample animsB 
  renderContent ctx (renderAnims anims) imageDB

slideshow :: (IsEventTarget document, IsDocument document)
          => CanvasRenderingContext2D
          -> document
          -> ImageDB
          -> Int
          -> IO ()
slideshow ctx doc imageDB fps = do
  Inputs { clicks=clicks, keyPressed=keyPressed, next=next, previous=previous } <- generateInputs doc
  tick <- generateTick fps

  timeRef <- initTime

  let t1 = makeTween' tweenTranslate (100, 100) (600, 500)
  let t2 = makeTween' (tweenColor FillColor) (RGB 255 0 0) (RGB 0 255 0)
  
  let anims = Anims [Anim 0.0 easeSin t1 2000000, Anim 0.0 easeSin t2 1000000] $ FCircle 100

  let updateA' = ((pure updateAnims) <@> tick)

  a <- accumB anims updateA'

  let loop = do
        -- clear screen
        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 
        
        renderAnimsB ctx imageDB a

        dtMs <- updateTime timeRef -- time in ms since last update
        let diff = (floor $ (1 / fromIntegral fps) * 1000) - dtMs
        when (diff > 0) $ do
          threadDelay $ diff * 1000 -- convert to microsecs and delay by that amount
        loop
  loop


-- data Stage = Stage

-- type SlideM = StateT (Int, AnimControl) ReaderT [Stage] IO ()


-- renderSlide :: SlideM -> IO ()
-- renderSlide 
