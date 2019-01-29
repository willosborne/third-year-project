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
import GHCJS.DOM.Types hiding (Text, Event, Animation)
import GHCJS.DOM.GlobalEventHandlers hiding (error)
import GHCJS.DOM.CanvasRenderingContext2D
  

import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Data.Typeable

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


-- tweenX :: Double -> Double -> AnimControl -> Content -> Content
-- tweenX x0 x1 t c = Translate x 200 c
--   where
--     x = lerp x0 x1 t


-- tweenTranslate :: (Double, Double) -> (Double, Double) -> AnimControl -> Content -> Content
-- tweenTranslate = tween (pair Translate)

-- tweenScale :: (Double, Double) -> (Double, Double) -> AnimControl -> Content -> Content
-- tweenScale = tween (pair Scale)

-- partially uncurry function to use a pair for interpolation over multiple things
pair :: (a -> a -> b -> b) -> ((a, a) -> b -> b)
pair f = \(x1, x2) b -> f x1 x2 b

pairI :: (Double -> Double -> b -> b) -> (I -> b -> b)
pairI f = \p b -> case p of
  (PairI (x, y)) -> f x y b
  DefaultI -> f 0 0 b
  g -> error $ "Must pass PairI. Got " ++ (show (typeOf g))

doubleI :: (Double -> b -> b) -> (I -> b -> b)
doubleI f = \p b -> case p of
  (DoubleI x) -> f x b
  DefaultI -> f 0 b
  _ -> error "Must pass DoubleI."

intI :: (Int -> b -> b) -> (I -> b -> b)
intI f = \i b -> case i of
  (IntI x) -> f x b
  DefaultI -> f 0 b
  _ -> error "Must pass IntI."

colorI :: (Color -> b -> b) -> (I -> b -> b)
colorI f = \c b -> case c of
  (ColorI col) -> f col b
  DefaultI -> f black b
  g -> error $ "Must pass ColorI. Got " ++ (show g)

-- tweenColor :: (Color -> Content -> Content) -- property to tween - e.g. Translate
--           -> Color -> Color -- start and end
--           -> AnimControl
--           -> Content
--           -> Content
-- tweenColor = tween

tween :: (I -> Content -> Content)
      -> I -> I
      -> AnimControl
      -> Content
      -> Content
tween property v0 v1 t c = property v c
  where
    v = interpolate' v0 v1 t
-- tween :: (Interpolate interp)
--       => (interp -> Content -> Content)
--       -> interp -> interp
--       -> AnimControl
--       -> Content
--       -> Content
-- tween property v0 v1 t c = property v c
--   where
--     v = interpolate v0 v1 t

-- sample an Animation behaviour and render it, all in one go.
renderAnimationB :: CanvasRenderingContext2D -> ImageDB -> Behaviour (Animation) -> IO ()
renderAnimationB ctx imageDB animsB = do
  anims <- sync $ sample animsB 
  renderContent ctx (renderAnimation anims) imageDB

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

  -- also need to somehow separate anims out between objects
  -- let anim1 = Animation [ makeTween tweenTranslate (100, 100) (600, 500) easeSin 2000000
  --                       , makeTween (tween FillColor) (RGB 255 0 0) (RGB 0 0 255) easeSin 2000000
  --                       , makeTween (tween StrokeWidth) 0 10 easeSin 2000000 ] $ FCircle 100
  -- let anim1 = Animation [ makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
  --                       , makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 0 255)) easeSin 2000000
  --                       , makeTween (tween (doubleI StrokeWidth)) (DoubleI 0) (DoubleI 10) easeSin 2000000 ] $ FCircle 100
  -- let animChain = chainAnimations [ chainTween (tween (pairI Translate)) (PairI (400, 100)) easeSin 2000000
  --                                 , chainTween (tween (colorI FillColor)) (ColorI (RGB 0 255 0)) easeSin 2000000
  --                                 , chainTween (tween (doubleI StrokeWidth)) (DoubleI 0) easeSin 2000000 ] anim1
  let anim1 = Animation [
        makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
        , makeTween (tween (pairI Scale)) (PairI (1, 1)) (PairI (2, 2)) easeSin 2000000
        , makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 2000000
        -- , makeTween (tween (doubleI StrokeWidth)) (DoubleI 0) (DoubleI 10) easeSin 2000000
        ] $ FRect 100 100
  let animChain = chainAnimations [
        chainTween (tween (pairI Translate)) (PairI (400, 100)) easeSin 2000000
        -- , chainTween (tween (pairI Scale)) (PairI (1, 1)) easeSin 2000000
        -- , chainTween (tween (colorI FillColor)) (ColorI (RGB 0 0 255)) easeSin 2000000
        , chainTween (tween (doubleI StrokeWidth)) (DoubleI 10) easeSin 2000000
        ] anim1

  -- let updateA' = (pure updateAnimation) <@> tick
  let updateA' = updateAnimation <$> tick

  a <- accumB anim1 updateA'
  a' <- accumB animChain updateA'
  -- b <- accumB anim2 updateA'

  -- NOTE: could use updates to reset animation on next event

  -- renderedAnim <- switchB


  let loop = do
        -- clear screen
        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 
        
        renderAnimationB ctx imageDB a
        renderAnimationB ctx imageDB a'
        -- renderAnimationB ctx imageDB b

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
