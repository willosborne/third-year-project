{-# LANGUAGE FlexibleInstances, GADTs, ExistentialQuantification #-}
module Animation where

import Content

import GHCJS.DOM.Types hiding (Text, Event, Animation)
import GHCJS.DOM.Document()
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers hiding (error)
import GHCJS.DOM.CanvasRenderingContext2D

import Web.KeyCode (keyCodeLookup, Key(..))

import Render
import ImagePreloader
import GHCJSTime
import Input

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
import Control.Monad.Writer
  
import Data.Foldable (foldrM, find)
import Data.List (deleteBy)
import qualified Data.Map as M
-- import Data.Maybe (fromMaybe)

-- import Reactive.Types
import Reactive.FRPSimple
-- import Reactive.React


import Debug.Trace
import Data.Typeable

import Data.Unique

  
type AnimControl = Double

clampAnimControl :: AnimControl -> AnimControl
clampAnimControl = max 0 . min 1


type Ease = AnimControl -> AnimControl

easeLinear :: Ease
easeLinear = id

easeSin :: Ease
easeSin t = sin (t * pi/2)

lerp :: (RealFrac a) => a -> a -> AnimControl -> a
lerp v0 v1 t = v0 + (v1 - v0) * (realToFrac t)

-- lerp :: Double -> Double -> AnimControl -> Double
-- lerp v0 v1 t = v0 + (v1 - v0) * t
lerpI :: Int -> Int -> AnimControl -> Int
lerpI v0 v1 t = v0 + (floor (fromIntegral (v1 - v0) * t))

-- transition :: (Double -> Content -> Content) -> Double -> Double -> Ease -> AnimControl -> Content -> Content
-- transition modifier v0 v1 ease t c = modifier v c
--   where
--     v = lerp v0 v1 $ ease t

class Interpolate a where
  -- |Interpolate between one value and another
  interpolate :: a -> a -> AnimControl -> a
  -- defaultVal :: a

-- data AnyInterpolate = forall a. Interpolate a => AnyInterpolate a

-- instance Interpolate AnyInterpolate where
--   interpolate (AnyInterpolate a1) (AnyInterpolate a2) t  = interpolate a1 a2 t
data I = PairI (Double, Double)
       | DoubleI Double
       | IntI Int
       | ColorI Color
       | DefaultI
       deriving (Show, Eq)

interpolate' :: I -> I -> AnimControl -> I
interpolate' (DoubleI d1) (DoubleI d2) t = DoubleI $ lerp d1 d2 t
interpolate' (IntI i1) (IntI i2) t = IntI $ floor $ lerp (fromIntegral i1) (fromIntegral i2) t
interpolate' (PairI (x1, y1)) (PairI (x2, y2)) t = PairI $ (lerp x1 x2 t, lerp y1 y2 t)
interpolate' (ColorI (RGB r1 g1 b1)) (ColorI (RGB r2 g2 b2)) t = ColorI $
                                                                 RGB
                                                                 (lerpI r1 r2 t)
                                                                 (lerpI g1 g2 t)
                                                                 (lerpI b1 b2 t)
interpolate' (ColorI (RGBA r1 g1 b1 a1)) (ColorI (RGBA r2 g2 b2 a2)) t = ColorI $
                                                                         RGBA
                                                                         (lerpI r1 r2 t)
                                                                         (lerpI g1 g2 t)
                                                                         (lerpI b1 b2 t)
                                                                         (lerp a1 a2 t)

interpolate' (ColorI (RGBA r1 g1 b1 a1)) (ColorI (RGB r2 g2 b2)) t = interpolate'
                                                                     (ColorI (RGBA r1 g1 b1 a1))
                                                                     (ColorI (RGBA r2 g2 b2 1.0))
                                                                     t
interpolate' (ColorI (RGB r1 g1 b1)) (ColorI (RGBA r2 g2 b2 a2)) t = interpolate'
                                                                     (ColorI (RGBA r1 g1 b1 1.0))
                                                                     (ColorI (RGBA r2 g2 b2 a2))
                                                                     t
interpolate' DefaultI DefaultI _ = DefaultI
interpolate' a b _ = error ("Mismatched interps. Interps: " ++ (show a) ++ ", " ++ (show b))

defaultI :: I -> I
defaultI (DoubleI _) = DoubleI 0
defaultI (IntI _) = IntI 0
defaultI (PairI _) = PairI (0, 0)
defaultI (ColorI _) = ColorI black
defaultI DefaultI = DefaultI

-- data Interpolate' a where
--   DoubleI :: Double -> Interpolate' Double
--   IntI :: Int -> Interpolate' Int
--   PairI :: (Double, Double) -> Interpolate' (Double, Double)
--   ColorI :: Color -> Interpolate' Color
  
-- data AnyInterpolate = forall a. AnyInterpolate (Interpolate' a)

-- interpolate' :: AnyInterpolate -> AnyInterpolate -> AnimControl -> AnyInterpolate
-- interpolate' (AnyInterpolate i1) (AnyInterpolate i2) t = case i1 of
--   DoubleInterpolate {} -> case 

instance Interpolate Double where
  interpolate = lerp
  -- defaultVal = 0.0

instance Interpolate Float where
  interpolate = lerp
  -- defaultVal = 0.0

instance Interpolate Int where
  interpolate v0 v1 t = floor $ lerp (fromIntegral v0) (fromIntegral v1) t
  -- defaultVal = 0

-- instance Interpolate (Double -> Content -> Content) where
--   interpolate v0 v1 t f = 

instance Interpolate (Double, Double) where
  interpolate (x0, y0) (x1, y1) t = (lerp x0 x1 t, lerp y0 y1 t)
  -- defaultVal = (0.0, 0.0)

instance Interpolate Color where
  interpolate (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) t = (RGBA
                                                          (interpolate r1 r2 t)
                                                          (interpolate g1 g2 t)
                                                          (interpolate b1 b2 t)
                                                          (interpolate a1 a2 t))
  interpolate (RGB r1 g1 b1) (RGB r2 g2 b2) t = (RGB
                                                  (interpolate r1 r2 t)
                                                  (interpolate g1 g2 t)
                                                  (interpolate b1 b2 t))
  interpolate (RGBA _ _ _ _) (RGB _ _ _) _ = undefined
  interpolate (RGB _ _ _) (RGBA _ _ _ _) _ = undefined
  -- defaultVal = (RGB 0 0 0)

-- parameter, ease, render function, duration
-- data Anim = Anim AnimControl Ease (AnimControl -> Content) Int
data Tween = Tween AnimControl Ease (AnimControl -> Content -> Content) Int
type ChainTween = ((I -> I -> AnimControl -> Content -> Content), I, Ease, Int)

-- data Animation = Animation AnimControl Ease [(AnimControl -> Content -> Content)] Content Int
data Animation = Animation [Tween] Content

makeAnimation :: [Tween] -> Content -> Animation
makeAnimation = Animation


makeAnimationTagged :: [Tween] -> Content -> IO (Animation, Unique)
makeAnimationTagged tweens c = do
  key <- newUnique
  return ((makeAnimation tweens c), key)
  

-- take an anim, a delta time, and update the control value
updateTween :: Int -> Tween -> Tween
updateTween dt (Tween t ease c duration) = Tween t' ease c duration
  where
    t' = clampAnimControl $ t + dt'
    dt' = (fromIntegral dt) / (fromIntegral duration)

updateAnimation :: Int -> Animation -> Animation
updateAnimation dt (Animation as c) = Animation (map (updateTween dt) as) c

renderAnimation :: Animation -> Content
renderAnimation (Animation as c) = foldr ($) c (map renderA as)
  where
    renderA (Tween t ease f _) = f $ ease t

-- need to compare tweens for chaining
tweenType :: Tween -> TransformType
-- tweenType (Tween _ _ f _) = traceShowId $ tweenFuncType f
tweenType (Tween _ _ f _) = tweenFuncType f

-- NOTE: this is probably where the problem is.
-- it only tweens whatever type i slap in below.
-- f :: I -> I -> AnimControl -> Content -> Content 

-- e.g. if I pass in (pairI Translate) and then pass in a None willy-nilly, it won't pattern match in the pairI
-- could just adjust transformer functions to ignore Nones
-- issue is that type system cannot determine which I type it should receive
chainType :: ChainTween -> TransformType
chainType (f, _, _, _) = tweenFuncType (f DefaultI DefaultI)
-- chainType (f, _, _, _) = traceShowId $ tweenFuncType (f (PairI (0, 0)) (PairI (0, 0)))

tweenFuncType :: (AnimControl -> Content -> Content) -> TransformType
tweenFuncType f = case (f 0.0 Empty) of
  Scale     _ _ _ -> Scaling
  Translate _ _ _ -> Translation
  Rotate      _ _ -> Rotation
  FillColor   _ _ -> FillColorChange
  StrokeColor _ _ -> StrokeColorChange
  StrokeWidth _ _ -> StrokeWidthChange
  _               -> None

compareTweenType :: Tween -> Tween -> Bool
compareTweenType t1 t2 = (tweenType t1) == (tweenType t2)

  
traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = traceStack (show x) (return x)

traceMonadStr :: (Show a, Monad m) => String -> a -> m a
traceMonadStr s x = traceStack (s ++ show x) (return x)

-- NOTE THIS MIGHT BE A BAD IDEA
-- lock a transition function so it doesn't vary with time and always returns the same value
lockTransition :: (AnimControl -> Content -> Content)
               -> AnimControl
               -> (AnimControl -> Content -> Content)
lockTransition f t = \_ -> f t

lockTween :: AnimControl -> Tween -> Tween
lockTween val (Tween t ease f duration) = Tween t ease (lockTransition f val) duration

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _ []     = []
deleteFirst f (x:xs) = if f x
                       then xs
                       else x : deleteFirst f xs

-- just pass on the key we were given
chainAnimationsTagged :: [ChainTween] -> (Animation, Unique) -> IO (Animation, Unique)
chainAnimationsTagged chains (anim, key) = do
  return (chainAnimations chains anim, key)

chainAnimations :: [ChainTween]
                -> Animation
                -> Animation
chainAnimations chainsIn (Animation tweensIn c) = Animation newTweens c
  where
    newTweens = chainLoop tweenPairs chainPairs

    tweenPairs = zip tweensIn (map tweenType tweensIn)
    chainPairs = zip chainsIn (map chainType chainsIn)

    chainLoop :: [(Tween, TransformType)] -> [(ChainTween, TransformType)] -> [Tween]
    chainLoop tweens (ch:chs) = case chainFunc tweens ch of
      Just (newTween, remainingTweens) -> newTween : chainLoop remainingTweens chs
      -- if no tween is matched, just make a tween from the default value
      Nothing                          -> let ((f, v2, ease, duration), _) = ch -- extract current chain
                                              v1 = defaultI v2 -- get default value
                                          in (makeTween f v1 v2 ease duration) : chainLoop tweens chs
    -- no chains left - lock all remaining tweens
    chainLoop tweens [] = map (lockTween 1.0 . fst) tweens

    chainFunc :: [(Tween, TransformType)] -> (ChainTween, TransformType) -> Maybe (Tween, [(Tween, TransformType)])
    chainFunc tweens ((fChain, v2, ease, duration), chainTy) = do
      ((Tween _ _ fTween _), foundType) <- find (\(_, ty) -> ty == chainTy) tweens
      lastVal <- extractInterp fTween 1.0
      return $ (makeTween fChain lastVal v2 ease duration, deleteFirst (\(_, ty) -> ty == chainTy) tweens)


-- extract the interp data fed into the *outer layer* of the render function. 
extractInterp :: (AnimControl -> Content -> Content)
              -> AnimControl
              -> Maybe I
extractInterp f t = extract (f t Empty)
  where
    extract (Translate x y _)     = Just $ PairI (x, y)
    extract (Scale     x y _)     = Just $ PairI (x, y)
    extract (Rotate theta _)      = Just $ DoubleI theta
    extract (FillColor   color _) = Just $ ColorI color
    extract (StrokeColor color _) = Just $ ColorI color
    extract (StrokeWidth width _) = Just $ DoubleI width
    extract _                     = Nothing

chainTween :: (I -> I -> AnimControl -> Content -> Content)
           -> I
           -> Ease
           -> Int
           -> ChainTween
chainTween f v1 ease duration = (f, v1, ease, duration)

-- this makes a tween that generates a *transform*, not a content - so you still need to pass in a content
makeTweenFunction :: (I -> I -> AnimControl -> Content -> Content)
                  -> I
                  -> I
                  -> (AnimControl -> Content -> Content)
makeTweenFunction f v0 v1 = \t -> f v0 v1 t

makeTween :: (I -> I -> AnimControl -> Content -> Content)
          -> I
          -> I
          -> Ease
          -> Int
          -> Tween
makeTween render v0 v1 ease duration = Tween 0.0 ease tweenFunc duration
  where
    tweenFunc = makeTweenFunction render v0 v1

-- makeAnimation' :: (Interpolate interpolate)
--                => [(Tween, interpolate)]
--                -> TransformState
--                -> Content
--
-- makeAnimation' :: [(Tween, interpolate)]
--                -> TransformState
--                -> Content
--                -> Animation' interpolate
-- makeAnimation' pairs initialState c = Animation' tweens c initialState endState
--   where
--     tweens = map fst pairs
--     endState = modifyState initialState (map (\(tw, v) -> (tweenType tw, v)) pairs)

-- -- take a map of new values for the ones which need changing
-- -- replace old values with new values and keep everything else the same
-- modifyState :: (Interpolate interpolate)
--             => TransformState 
--             -> [(TransformType, interpolate)]
--             -> TransformState
-- modifyState (TransformState innerMap) pairs = finalValMap
--   where
--     stateChanges = M.fromList pairs
--     finalValMap = TransformState $ M.union stateChanges innerMap

-- also outputs its final state
-- makeTween' :: (Interpolate interpolate)
--            => (interpolate -> interpolate -> AnimControl -> Content -> Content)
--            -> interpolate
--            -> interpolate
--            -> Ease
--            -> Int
--            -> (Tween, interpolate)
-- makeTween' render v0 v1 ease duration = (Tween 0.0 ease tweenFunc duration, v1)
--   where
--     tweenFunc = makeTweenFunction render v0 v1
        
