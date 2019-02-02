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
import Data.Unique
import Data.List (nub)
import Data.List.Index (imap, imapM)
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

tween :: (I -> Content -> Content)
      -> I -> I
      -> AnimControl
      -> Content
      -> Content
tween property v0 v1 t c = property v c
  where
    v = interpolate' v0 v1 t

-- sample an Animation behaviour and render it, all in one go.
renderAnimationB :: CanvasRenderingContext2D -> ImageDB -> Behaviour Animation -> IO ()
renderAnimationB ctx imageDB animB = do
  anim <- sync $ sample animB 
  renderContent ctx (renderAnimation anim) imageDB

renderTaggedIndexedAnimationB :: CanvasRenderingContext2D
                              -> ImageDB
                              -> Behaviour (Int, (Animation, Unique))
                              -> IO ()
renderTaggedIndexedAnimationB ctx imageDB animB = do
  (_, (anim, _)) <- sync $ sample animB
  renderContent ctx (renderAnimation anim) imageDB

renderAnimationBListB :: CanvasRenderingContext2D
                     -> ImageDB
                     -> Behaviour [Behaviour (Int, (Animation, Unique))]
                     -> IO ()
renderAnimationBListB ctx imageDB animsBListB = do
  animsBList <- sync $ sample animsBListB
  mapM_ (renderTaggedIndexedAnimationB ctx imageDB) animsBList

updateTaggedIndexedAnimation :: Int -> (Int, (Animation, Unique)) -> (Int, (Animation, Unique))
updateTaggedIndexedAnimation dt (i, (anim, key)) = (i, (updateAnimation dt anim, key))

indexOrLast :: [a] -> Int -> a
indexOrLast list i
  | i < length list = list !! i
  | otherwise       = last list

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe list i
  | i < length list = Just $ list !! i
  | otherwise       = Nothing

slide :: (IsEventTarget document, IsDocument document)
          => CanvasRenderingContext2D
          -> document
          -> ImageDB
          -> Int
          -> [(Animation, Unique)]
          -> IO ()
slide ctx doc imageDB fps anims = do
  Inputs { clicks=clicks, keyPressed=keyPressed, next=next, previous=previous } <- generateInputs doc
  tick <- generateTick fps
  timeRef <- initTime

  -- let anim1 = Animation [
  --       makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
  --       , makeTween (tween (pairI Scale)) (PairI (1, 1)) (PairI (2, 2)) easeSin 2000000
  --       , makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 2000000
  --       -- , makeTween (tween (doubleI StrokeWidth)) (DoubleI 0) (DoubleI 10) easeSin 2000000
  --       ] $ FRect 100 100
  -- let animChain = chainAnimations [
  --       chainTween (tween (pairI Translate)) (PairI (400, 100)) easeSin 2000000
  --       -- , chainTween (tween (pairI Scale)) (PairI (1, 1)) easeSin 2000000
  --       -- , chainTween (tween (colorI FillColor)) (ColorI (RGB 0 0 255)) easeSin 2000000
  --       , chainTween (tween (doubleI StrokeWidth)) (DoubleI 10) easeSin 2000000
  --       ] anim1
  -- let animChain2 = chainAnimations [chainTween (tween (pairI Translate)) (PairI (800, 300)) easeSin 2000000] animChain

  -- let anims = [anim1, animChain, animChain2]

  -- -- behaviour containing current index value
  -- currentAnimIndexB <- accumB 0 ((+1) <$ clicks)
  -- -- event for the same, fires whenever index value changes
  -- -- currentAnimIndexE <- accumE 0 ((+1) <$ clicks)

  -- -- tickIfActive is an event that ticks only when shouldTickB is currently True
  -- let tickIfActive shouldTickB = whenE shouldTickB (tick)
  --     -- update the anim only if shouldTickB is currently true
  --     updateIfActiveE shouldTickB = whenE shouldTickB (updateAnimation <$> tick)
  --     -- this takes an index and returns a behaviour that's true whenever currentAnimIndexB is equal to that index
  --     shouldTick i = (i ==) <$> currentAnimIndexB

  -- -- list of behaviours that accumulate correct state
  -- -- TODO: just write my own version of this because I definitely don't need another dependency for this function
  -- animsAccum <- imapM (\i anim -> accumB anim (updateIfActiveE (shouldTick i))) anims
  -- -- let currentAnimB = switchB $ (animsAccum !!) <$> currentAnimIndexB
  -- let currentAnimB = switchB $ (indexOrLast animsAccum) <$> currentAnimIndexB

  -- unique classes
  -- let classes = nub $ map snd anims
  let animsIndexed = imap (\i pair -> (i, pair)) anims
  putStrLn $ show $ map fst animsIndexed

  -- a list of (Int, (Animation, Unique)) pairs

  -- conditions for tick and render:
  -- index < currentIndex
  -- there is no other anim2 with the same class where index(anim2) > index

  -- all animations with the same key as the given anim and an index <= current index OTHER than the one we search for 
  let filteredAnims :: (Int, (Animation, Unique)) -> Int -> [(Int, (Animation, Unique))]
      filteredAnims (index, (anim, key)) currentI = filter (\(i, (a, k)) -> (k == key) &&
                                                                            (i <= currentI) &&
                                                                            (i /= index)) animsIndexed
      
      animActive :: (Int, (Animation, Unique)) -> Int -> Bool                                                    
      animActive whole@(i, anim) currentI = (i <= currentI) && largestVal filtered
        where
          largestVal [] = True
          largestVal ((i', _):xs)
            | i' > i = False
            | otherwise = largestVal xs
          filtered = filteredAnims whole currentI
          
      -- filterActive :: [(Int, (Animation, Unique))] -> Int ->[(Int, (Animation, Unique))]
      -- filterActive as i = filter (\a -> animActive a i) as
      -- filterActive :: [Behaviour (Int, (Animation, Unique))] -> Int -> [(Int, (Animation, Unique))]
      -- filterActive as iB = do
      --   i <- sync $ sample iB
      --   return $ filter (\a -> animActive a i) as

  -- behaviour containing current index value
  currentAnimIndexB <- accumB 0 ((+1) <$ clicks)

  _ <- listenToBehaviour currentAnimIndexB (\new -> liftIO $ putStrLn (show new)) 
  -- event for the same, fires whenever index value changes
  -- currentAnimIndexE <- accumE 0 ((+1) <$ clicks)

  -- update the anim only if shouldTickB is currently true
  let updateIfActiveE shouldTickB = whenE shouldTickB (updateTaggedIndexedAnimation <$> tick)
      -- this takes an index and returns a behaviour that's true whenever currentAnimIndexB is equal to that index
      shouldTick anim = (animActive anim) <$> currentAnimIndexB

  -- list of behaviours that accumulate correct state
  animsB <- mapM (\anim -> accumB anim (updateIfActiveE (shouldTick anim))) animsIndexed
  -- animsB :: [Behaviour (Int, (Animation, Unique))]

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  -- f <$> b = pure f <*> b
  -- filterActive :: [(...)] -> Int -> [(...)]
  -- let currentAnimsB = filterActive <$> animsB
  -- let b = currentAnimsB <*> currentAnimIndexB

  -- AnimData :: (Int, (Animation, Unique))

  -- fmap filterActive :: Functor f => f [(...)] -> f [f (...)] 
  -- fmap (fmap filterActive) :: Functor f => [Behaviour (...)] -> [Behaviour (f (...))]
  -- fmap (fmap filterActive) animsB :: [Behaviour (Int -> )]
  
  -- fmap (fmap animActive) :: Functor f => [Behaviour (...)] -> [Behaviour (f (...))]
  -- fmap (fmap animActive) animsB :: [Behaviour (Int -> Bool)]
  -- fmap animActive :: f (...) -> f (Int -> Bool)
  -- fmap animActive (f AnimData) :: f (Int -> Bool)
  -- fmap animActive (f AnimData) <*> currentAnimIndexB :: f Bool
  -- currentAnimsB :: Behaviour [Behaviour (Int, (Animation, Unique))]
  -- this is a behaviour containing the set of currently active animations (i.e. the set to render)
  let currentAnimsB = filterM (\animB -> (animActive <$> animB) <*> currentAnimIndexB) animsB

  let loop = do
        clearScreen ctx

        -- renderAnimationB ctx imageDB currentAnimB
        renderAnimationBListB ctx imageDB currentAnimsB

        dtMs <- updateTime timeRef -- time in ms since last update
        let diff = (floor $ (1 / fromIntegral fps) * 1000) - dtMs
        when (diff > 0) $ do
          threadDelay $ diff * 1000 -- convert to microsecs and delay by that amount
        loop
  loop
