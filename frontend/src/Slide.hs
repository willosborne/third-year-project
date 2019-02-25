{-# LANGUAGE NamedFieldPuns, RecursiveDo #-}
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

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Unique
  
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer hiding (listen)
import Control.Concurrent
import Control.Applicative

data Inputs = Inputs {
  clicks :: Event (Int, Int),
  keyPressed :: Event Key,
  next :: Event (),
  previous :: Event (),
  tick :: Event Int
  }

generateInputs :: (IsEventTarget document, IsDocument document)
               => document
               -> Int
               -> IO Inputs
generateInputs doc fps = do
  (click, sendClick) <- newEvent
  (keyPressed, sendKeyPressed) <- newEvent
  (mouseXY, sendMouseXY) <- newEvent
  (next, sendNext) <- newEvent
  (previous, sendPrevious) <- newEvent
  
  bMouseXY <- hold (0, 0) mouseXY
  let clicks = bMouseXY <@ click

  tick <- generateTick fps

  _ <- on doc mouseUp $ do
    -- (x, y) <- mouseClientXY
    -- Just button <- toMouseButton <$> mouseButton
    liftIO $ sync $ do
      sendClick ()
      sendNext ()

  _ <- on doc mouseMove $ do
    xy <- mouseClientXY
    liftIO $ sync $ sendMouseXY xy

  _ <- on doc keyDown $ do
    key <- keyCodeLookup . fromIntegral <$> uiKeyCode
    repeating <- uiKeyRepeat
    liftIO $ sync $ when (not repeating) $ do
      sendKeyPressed key
      case key of
        ArrowLeft -> sendPrevious ()
        ArrowRight -> sendNext ()
        Space -> sendNext ()
        _ -> return ()
    

  return Inputs {clicks=clicks, keyPressed=keyPressed, next=next, previous=previous, tick=tick}

-- |Return a tick event that fires based on the FPS, returning the time since last tick each time in milliseconds
generateTick :: Int -> IO (Event Int)
generateTick fps = do
  (tick, sendTick) <- newEvent

  let delay = floor $ (1 / (fromIntegral fps)) * 1000000

  _ <- forkIO $ forever $ do
    threadDelay delay
    sync $ sendTick delay

  return tick

-- transformer functions - we need these to convert between Content functions and I values
pairI :: (Float -> Float -> b -> b) -> (I -> b -> b)
pairI f = \p b -> case p of
  (PairI (x, y)) -> f x y b
  DefaultI -> f 0 0 b
  g -> error $ "Must pass PairI. Got " ++ (show g)

floatI :: (Float -> b -> b) -> (I -> b -> b)
floatI f = \p b -> case p of
  (FloatI x) -> f x b
  DefaultI -> f 0 b
  g -> error $ "Must pass FloatI. Got " ++ (show g)

intI :: (Int -> b -> b) -> (I -> b -> b)
intI f = \i b -> case i of
  (IntI x) -> f x b
  DefaultI -> f 0 b
  g -> error $ "Must pass IntI. Got " ++ (show g)

colorI :: (Color -> b -> b) -> (I -> b -> b)
colorI f = \c b -> case c of
  (ColorI col) -> f col b
  DefaultI -> f black b
  g -> error $ "Must pass ColorI. Got " ++ (show g)

-- |Perform necessary interpolations on the tween and call the render function.
tween :: (I -> Content -> Content)
      -> I -> I
      -> AnimControl
      -> Content
      -> Content
tween property v0 v1 t c = property v c
  where
    v = interpolate v0 v1 t

-- helper functions for common tween operations
tweenTranslate   :: I -> I -> AnimControl -> Content -> Content
tweenTranslate    = tween $ pairI Translate

tweenScale       :: I -> I -> AnimControl -> Content -> Content
tweenScale        = tween $ pairI Scale

tweenFillColor   :: I -> I -> AnimControl -> Content -> Content
tweenFillColor    = tween $ colorI FillColor

tweenStrokeColor :: I -> I -> AnimControl -> Content -> Content
tweenStrokeColor  = tween $ colorI StrokeColor

tweenStrokeWidth :: I -> I -> AnimControl -> Content -> Content
tweenStrokeWidth  = tween $ floatI StrokeWidth

tweenRotate      :: I -> I -> AnimControl -> Content -> Content
tweenRotate       = tween $ floatI Rotate

-- |Sample a 'Behaviour Animation' and render it, all in one go.
renderAnimationB :: CanvasRenderingContext2D -> ImageDB -> Behaviour Animation -> IO ()
renderAnimationB ctx imageDB animB = do
  anim <- sync $ sample animB 
  renderContent ctx (renderAnimation anim) imageDB

-- |Sample a 'Behaviour' containing a tagged and indexed 'Animation' and render it.
renderTaggedIndexedAnimationB :: CanvasRenderingContext2D
                              -> ImageDB
                              -> Behaviour (Int, TaggedAnimation)
                              -> IO ()
renderTaggedIndexedAnimationB ctx imageDB animB = do
  (_, (anim, _)) <- sync $ sample animB
  renderContent ctx (renderAnimation anim) imageDB

-- |Sample and render a list of tagged, indexed animation 'Behaviour's.
renderAnimationBListB :: CanvasRenderingContext2D
                     -> ImageDB
                     -> Behaviour [Behaviour (Int, TaggedAnimation)]
                     -> IO ()
renderAnimationBListB ctx imageDB animsBListB = do
  animsBList <- sync $ sample animsBListB
  mapM_ (renderTaggedIndexedAnimationB ctx imageDB) animsBList

-- |Like 'updateAnimation', but for tagged, indexed versions.
updateTaggedIndexedAnimation :: Int -> (Int, TaggedAnimation) -> (Int, TaggedAnimation)
updateTaggedIndexedAnimation dt (i, (anim, key)) = (i, (updateAnimation dt anim, key))

indexOrLast :: [a] -> Int -> a
indexOrLast list i
  | i >= 0 && i < length list = list !! i
  | otherwise                 = last list

makeSlide :: [(Animation, Unique)]
          -> CanvasRenderingContext2D
          -> document
          -> ImageDB
          -> Inputs
          -> Behaviour Bool
          -> Int
          -> IO ((IO (), Event (), Event ())) -- render, previous slide, next slide
makeSlide anims ctx _ imageDB inputs _ fps = do
  let Inputs { next, previous, tick } = inputs
  
  timeRef <- initTime

  let animsIndexed = zip [0..] anims

  -- conditions for tick and render:
  -- index < currentIndex
  -- there is no other anim2 with the same class where index(anim2) > index

  -- all animations with the same key as the given anim and an index <= current index OTHER than the one we search for 
  let filteredAnims :: (Int, TaggedAnimation) -> Int -> [(Int, TaggedAnimation)]
      filteredAnims (index, (_, key)) currentI = filter (\(i, (_, k)) -> (k == key) &&
                                                                            (i <= currentI) &&
                                                                            (i /= index)) animsIndexed
      
      animActive :: (Int, TaggedAnimation) -> Int -> Bool                                                    
      animActive whole@(i, _) currentI = (i <= currentI) && largestVal filtered
        where
          largestVal [] = True
          largestVal ((i', _):xs)
            | i' > i = False
            | otherwise = largestVal xs
          filtered = filteredAnims whole currentI

  let stepV = ((1 <$ next) <> ((-1) <$ previous))
      step  = (+) <$> stepV
      stepM = (*) <$> stepV
  stepB <- hold (*1) stepM

  -- behaviour containing current index value
  currentAnimIndexB <- accumB 0 step
  -- event for the same, fires whenever index value changes
  currentAnimIndexE <- accumE 0 step

  let nextSlideE = () <$ filterE (>= (length anims)) currentAnimIndexE
  let prevSlideE = () <$ filterE (< 0) currentAnimIndexE

  -- update the anim only if shouldTickB is currently true
  let updateIfActiveE shouldTickB = whenE shouldTickB (updateTaggedIndexedAnimation <$> (stepB <@> tick))
      -- this takes an index and returns a behaviour that's true whenever currentAnimIndexB is equal to that index
      shouldTick anim = (animActive anim) <$> currentAnimIndexB

  -- list of behaviours that accumulate correct state
  animsB <- mapM (\anim -> accumB anim (updateIfActiveE (shouldTick anim))) animsIndexed
  -- animsB :: [Behaviour (Int, (Animation, Unique))]

  -- this is a behaviour containing the set of currently active animations (i.e. the set to render)
  -- filter out animations that aren't currently active.
  -- here, liftA2 allows us to pass animActive (no concept of Behaviours) and two Behaviour-wrapped values
  -- alternative is animActive <$> animB <*> currentAnimIndexB
  let currentAnimsB = filterM (\animB -> liftA2 animActive animB currentAnimIndexB) animsB

  let loop = do
        clearScreen ctx

        renderAnimationBListB ctx imageDB currentAnimsB

        dtMs <- updateTime timeRef -- time in ms since last update
        let diff = (floor $ (1 / fromIntegral fps) * 1000) - dtMs
        when (diff > 0) $ do
          threadDelay $ diff * 1000 -- convert to microsecs and delay by that amount
  return (loop, prevSlideE, nextSlideE)

-- |A function that can be run as part of a 'slideshow' call.
-- Takes a canvas context, a document, an 'ImageDB', an 'Inputs' object, a 'paused' behaviour, and an FPS.
-- Must return an IO action to be called when it's rendered, and two 'Event's - 'nextSlide' and 'previousSlide'.
type SlideFunc document = CanvasRenderingContext2D -> document -> ImageDB -> Inputs -> Behaviour Bool -> Int -> IO (IO (), Event (), Event ())

-- we collect a list of rendering functions - animations etc are already passed in
type SlideWriter document = WriterT [SlideFunc document]
                            IO ()

slide :: (IsEventTarget document, IsDocument document)
      => AnimWriter
      -> SlideWriter document
slide animWriter = do
  anims <- liftIO $ execWriterT animWriter
  let out = makeSlide anims
  tell [out]
  
slideList :: (IsEventTarget document, IsDocument document)
          => [TaggedAnimation]
          -> SlideWriter document
slideList anims = do
  let out = makeSlide anims
  tell [out]

slideGeneric :: (IsEventTarget document, IsDocument document)
             => SlideFunc document
             -> SlideWriter document
slideGeneric func = tell [func]

-- generate a new Inputs with events that only fire when index is right
modifyInputs :: Inputs -> Behaviour Int -> Int -> Inputs
modifyInputs (Inputs {clicks,
                      keyPressed,
                      next,
                      previous,
                      tick}) currentIndexB i = out
  where
    out = Inputs { clicks=clicks',
                   keyPressed=keyPressed',
                   next=next',
                   previous=previous',
                   tick=tick' }
    clicks' = whenE shouldTickB clicks
    keyPressed' = whenE shouldTickB keyPressed
    next' = whenE shouldTickB next
    previous' = whenE shouldTickB previous
    tick' = whenE shouldTickB tick
    shouldTickB = (i ==) <$> currentIndexB

-- |Take a SlideWriter do block and necessary control vars and run a slideshow.
slideshow :: (IsEventTarget document, IsDocument document)
          => CanvasRenderingContext2D
          -> document
          -> ImageDB
          -> Int
          -> SlideWriter document
          -> IO ()
slideshow ctx doc imageDB fps slideWriter = mdo
  -- NOTE: use of recursive do to avoid circular definition
  slideFuncs <- execWriterT slideWriter
  baseInputs <- generateInputs doc fps

  slides <- mapM (\(f, i) -> f ctx doc imageDB
                           (modifyInputs baseInputs currentIndexB i)
                           ((i ==) <$> currentIndexB)
                           fps) (zip slideFuncs [0..])
  let action (a, _, _) = a
      prev   (_, p, _) = p
      next   (_, _, n) = n

      nextE = foldl' (<>) never (map next slides)
      prevE = foldl' (<>) never (map prev slides)
      step = (clampSlide . (+1) <$ nextE) <> (clampSlide . (subtract 1) <$ prevE)
      clampSlide i = min ((length slides) - 1) $ max 0 i

  currentIndexB <- accumB 0 step
  let currentSlideB = (slides !!) <$> currentIndexB

  let loop = do
        currentSlide <- sync $ sample currentSlideB
        action currentSlide
        loop
  loop
