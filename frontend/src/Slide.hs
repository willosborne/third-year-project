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

import Control.Monad
import Control.Monad.IO.Class

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

  forkIO $ forever $ do
    threadDelay delay
    sendTick delay




slideshow :: (IsEventTarget document, IsDocument document)
          => document
          -> ImageDB
          -> Int
          -> IO ()
slideshow doc imageDB fps = do
  Inputs { clicks=clicks, keyPressed=keyPressed, next=next, previous=previous } <- generateInputs doc
  tick <- generateTick fps

  timeRef <- initTime

  -- let anim = Anim 0.0 easeSin (

  a <- accumB 

  anims :: Behaviour (Behaviour Content)

  let loop = do
        -- startTime <- getTime

        -- clear screen
        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        -- g <- sync $ sample bLife
        
        -- renderContent ctx (renderGrid g) em


        -- endTime <- getTime

        -- let diff = (realToFrac (1/30)) - (endTime - startTime)
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
