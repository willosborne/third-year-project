module Animation where

import Content

import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Document()
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.CanvasRenderingContext2D

import Web.KeyCode (keyCodeLookup)

import Render
import ImagePreloader
import GHCJSTime
import Input

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
  
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe)
  
type AnimControl = Double

-- easing function
lerp :: Double -> Double -> AnimControl -> Double
lerp v0 v1 t = v0 + (v1 - v0) * t

easeLinear :: AnimControl -> AnimControl
easeLinear = id

easeSin :: AnimControl -> AnimControl
easeSin t = sin (t * pi/2)

transition :: (Double -> Content -> Content) -> Double -> Double -> (AnimControl -> AnimControl) -> AnimControl -> Content -> Content
transition modifier v0 v1 ease t c = modifier v c
  where
    v = lerp v0 v1 $ ease t


-- simple version based on Shine
runProgram :: (IsEventTarget eventTarget, IsDocument eventTarget) 
           => CanvasRenderingContext2D
           -> eventTarget -- doc here, for registering events
           -> ImageDB
           -> state -- initial state
           -> (state -> IO Content) -- produce content from state
           -> (UserEvent -> state -> IO state) -- step world state based on an event
           -> (Double -> state -> IO state) -- update state based on time
           -> IO ()
runProgram ctx doc imageDB initialState renderState processEvent processTime = do
  initialTime <- getTime
  eventMVar <- newMVar [] -- mutable list to accumulate events as they occur

  -- register event handlers
  _ <- on doc mouseDown $ do
    Just button <- toMouseButton <$> mouseButton
    (x, y) <- mouseClientXY
    
    liftIO $ modifyMVar_ eventMVar $ fmap return (EventMouseDown x y button :)
  _ <- on doc keyDown $ do
    key <- keyCodeLookup . fromIntegral <$> uiKeyCode
    
    liftIO $ modifyMVar_ eventMVar $ fmap return (EventKeyDown key :)
  _ <- on doc keyUp $ do
    key <- keyCodeLookup . fromIntegral <$> uiKeyCode
    
    liftIO $ modifyMVar_ eventMVar $ fmap return (EventKeyUp key :)

  let loop stateIn = do
        -- putStrLn "loop start"
        startTime <- getTime

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        -- i don't know why this doesn't work!
        -- events <- fmap (fromMaybe []) $ tryTakeMVar eventMVar
        events <- modifyMVar eventMVar $ \xs -> return ([], xs)
        
        -- [event], state, (event -> state -> IO state)
        -- mapM_ (putStrLn . show) events
        -- mapM_ ((flip processEvent) stateIn) (fromMaybe [] events)
        inputState <- foldrM processEvent stateIn events
        newState <- processTime (startTime - initialTime) inputState

        c <- renderState newState
        runReaderT (render ctx c) imageDB

        endTime <- getTime

        let diff = (realToFrac (1/60)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000

        loop newState
        
  loop initialState



-- shine pointed me towards this animation system and MVars.
-- depends on time (crucially, not on user input)
animate :: CanvasRenderingContext2D
        -- -> IORef Int -- time ref
        -> ImageDB
        -> (Double -> IO Content)
        -> IO ()
animate ctx imageDB f = do
  -- timeRef <- initTime
  animStartTime <- getTime

  let loop = do
        startTime <- getTime
        -- dtStart <- updateTime timeRef

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        c <- f $ startTime - animStartTime
        runReaderT (render ctx c) imageDB

        -- dtEnd <- updateTime timeRef
        endTime <- getTime

        let diff = (realToFrac (1/60)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000
        loop
  
  loop
