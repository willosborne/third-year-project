module Animation where

import Content

import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.Document()
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.CanvasRenderingContext2D

import Web.KeyCode (keyCodeLookup, Key(..))

import Render
import ImagePreloader
import GHCJSTime
import Input

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
  
import Data.Foldable (foldrM)
-- import Data.Maybe (fromMaybe)

-- import Reactive.Types
import Reactive.FRPSimple
-- import Reactive.React

  
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

posChange :: Fractional a => UserEvent -> Maybe ((a, a) -> (a, a))
posChange (EventKeyDown key) = case key of
  ArrowLeft  -> Just $ \(x, y) -> (x - 10, y)
  ArrowRight -> Just $ \(x, y) -> (x + 10, y)
  _          -> Nothing
posChange _ = Nothing

-- posB :: Fractional a => Inputs -> Behaviour (a, a)
-- posB = accumB (0, 0) . filterJust . fmap posChange . userEvents

-- createContent :: Double -> Content
-- createContent angle = Translate 400 300 $ Rotate angle $ Image "yoda" Original

-- program :: Inputs -> Behaviour Content
-- program inputs = createContent <$> posB inputs



animateReact :: (IsEventTarget eventTarget, IsDocument eventTarget)
             => CanvasRenderingContext2D
             -> eventTarget
             -> ImageDB
             -- -> (Double -> IO Content)
             -> IO ()
animateReact ctx doc imageDB = do
  animStartTime <- getTime

  -- TODO mouse input should be a behaviour 
  (click, sendClick) <- newEvent
  (keyDown, sendKeyDown) <- newEvent
  (keyUp, sendKeyUp) <- newEvent

  _ <- on doc mouseDown $ do
    Just button <- toMouseButton <$> mouseButton
    (x, y) <- mouseClientXY
    liftIO $ sync $ sendClick $ EventMouseDown x y button
  -- _ <- on doc keyDown $ do
  --   key <- keyCodeLookup . fromIntegral <$> uiKeyCode
  --   liftIO $ sync $ sendKeyDown $ EventKeyDown key
  -- _ <- on doc keyUp $ do
  --   key <- keyCodeLookup . fromIntegral <$> uiKeyCode
  --   liftIO $ sync $ sendKeyUp $ EventKeyUp key

  let loop = do
        startTime <- getTime

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        -- runReaderT (render ctx c) imageDB

        endTime <- getTime

        let diff = (realToFrac (1/60)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000
        loop
  loop

animTest :: (IsEventTarget eventTarget, IsDocument eventTarget)
             => CanvasRenderingContext2D
             -> eventTarget
             -> ImageDB
             -- -> (Double -> IO Content)
             -> IO ()
animTest ctx doc imageDB = do
  animStartTime <- getTime

  (a, sendA) <- newEvent
  bA <- hold (0 :: Int) a

  bSumA <- accumB 0 ((+) <$> a)
  sumA <- accumE 0 ((+) <$> a)

  listen a (\new -> liftIO (putStrLn ("a is now: " ++ (show new))))
  listenToBehaviour bA (\new -> liftIO (putStrLn ("bA is now: " ++ (show new))))
  listen sumA (\new -> liftIO (putStrLn ("sumA is now: " ++ (show new))))
  listenToBehaviour bSumA (\new -> liftIO (putStrLn ("bSumA is now: " ++ (show new))))

  _ <- on doc mouseDown $ do
    Just button <- toMouseButton <$> mouseButton
    (x, y) <- mouseClientXY
    liftIO $ sync $ sendA x

  let loop = do
        startTime <- getTime

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        endTime <- getTime

        let diff = (realToFrac (1/60)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000
        loop
  loop

  

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
  animStartTime <- getTime

  let loop = do
        startTime <- getTime

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        c <- f $ startTime - animStartTime
        runReaderT (render ctx c) imageDB

        endTime <- getTime

        let diff = (realToFrac (1/60)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000
        loop
  loop
