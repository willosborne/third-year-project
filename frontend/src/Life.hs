{-# LANGUAGE NamedFieldPuns #-}
module Life where

import Reactive.FRPSimple
import ImagePreloader
import Content
import Input
import Render
import GHCJSTime
import Slide

import Web.KeyCode

import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.Document()
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.CanvasRenderingContext2D

import Data.Monoid
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import qualified Data.Sequence as S

type Grid = S.Seq (S.Seq Bool)

blank :: Int -> Int -> Grid
blank x y = S.replicate y (S.replicate x False)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [
  (x + dx, y + dy) |
  dx <- [-1..1], dy <- [-1..1],
  not (dx == 0 && dy == 0)
  ]


countLiveNeighbours :: (Int, Int) -> Grid -> Int
countLiveNeighbours point grid = foldl (flip ((+) . fromEnum)) 0 $ [
  val xy |
    xy <- neighbours point
  ]
  where
    val (x, y) = S.index (S.index grid (wrap y ymax)) (wrap x xmax)
    wrap v vmax
      | v < 0     = v + vmax
      | v > vmax  = v - vmax
      | otherwise = v
    
    xmax = (S.length (S.index grid 0)) - 1
    ymax = (S.length grid) - 1
  
step :: Grid -> Grid
step grid = mapXY (\x y alive -> let liveNeighbours = countLiveNeighbours (x, y) grid
                    in
                      case alive of
                        True | liveNeighbours < 2 -> False
                             | liveNeighbours > 3 -> False
                             | otherwise          -> True
                        False -> if (liveNeighbours == 3) then True else False) grid

modify :: (Int, Int) -> Grid -> Grid
modify (x, y) grid = S.adjust (S.adjust not x) y grid

start :: Grid
start = blank 35 27

cellSize :: Int
cellSize = 25

adjust :: (Int, Int) -> (Int, Int)
adjust (mouseX, mouseY) = (div mouseX cellSize, div mouseY cellSize)

mapXY :: (Int -> Int -> a -> b) -> S.Seq (S.Seq a) -> S.Seq (S.Seq b)
mapXY f grid = S.mapWithIndex (\y row -> S.mapWithIndex (\x val -> f x y val) row) grid

renderGrid :: Grid -> Content
renderGrid grid = Translate 12.5 12.5 $ foldr (<>) Empty $ fmap (foldr (<>) Empty) contentGrid
  where
    contentGrid = mapXY (\x y val -> renderCell x y val) grid
    renderCell x y True = Translate (fromIntegral $ x * cellSize) (fromIntegral $ y * cellSize) $ FillColor (RGB 0 0 0) $
      FRect (fromIntegral cellSize) (fromIntegral cellSize)
    renderCell x y False = Empty
                                     
lifeSlide :: (IsEventTarget target, IsDocument target) => SlideFunc target
lifeSlide ctx doc db inputs activeB fps = do
  timeRef <- initTime

  let Inputs {clicks, keyPressed, tick} = inputs
      pause = () <$ filterE (== Space) keyPressed
      nextE = () <$ filterE (== ArrowRight) keyPressed
      prevE = () <$ filterE (== ArrowLeft) keyPressed

  (tick, sendTick) <- newEvent

  active <- accumB False (not <$ pause)

  let steps = whenE active (step <$ tick)
      modifies = modify . adjust <$> clicks
      changes = steps <> modifies
  life <- accumE start changes

  bLife <- hold start life

  _ <- forkIO $ forever $ do
    threadDelay 200000
    active <- sync $ sample activeB
    when active $ sync $ sendTick ()

  let loop = do

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        g <- sync $ sample bLife
        
        renderContent ctx (renderGrid g) db


        dtMs <- updateTime timeRef
        let diff = (floor $ (1 / fromIntegral fps) * 1000) - dtMs
        when (diff > 0) $ do
          threadDelay $ diff * 1000 -- convert to microsecs and delay by that amount
  return (loop, prevE, nextE)

runLife :: (IsEventTarget target, IsDocument target)
        => CanvasRenderingContext2D
        -> target
        -> IO ()
runLife ctx doc = do
  animStartTime <- getTime

  -- TODO mouse input should be a behaviour 
  (click, sendClick) <- newEvent
  (pause, sendPause) <- newEvent
  (tick, sendTick) <- newEvent
  (mouseXY, sendMouseXY) <- newEvent

  bMouseXY <- hold (0, 0) mouseXY
  let clicks = bMouseXY <@ click

  active <- accumB False (not <$ pause)

  let steps = whenE active (step <$ tick)
      modifies = modify . adjust <$> clicks
      changes = steps <> modifies
  life <- accumE start changes

  bLife <- hold start life

  _ <- forkIO $ forever $ do
    threadDelay 200000
    putStrLn "Tick!"
    sync $ sendTick ()

  _ <- on doc mouseDown $ do
    (x, y) <- mouseClientXY
    liftIO $ sync $ sendClick ()

  _ <- on doc mouseMove $ do
    xy <- mouseClientXY
    liftIO $ sync $ sendMouseXY xy

  _ <- on doc keyUp $ do
    key <- keyCodeLookup . fromIntegral <$> uiKeyCode
    liftIO $ when (key == Space) $ sync $ sendPause ()

  
  em <- emptyDB

  let loop = do
        startTime <- getTime

        clearRect ctx 0 0 6000 4000
        setTransform ctx 1 0 0 1 0 0 

        g <- sync $ sample bLife
        
        renderContent ctx (renderGrid g) em


        endTime <- getTime

        let diff = (realToFrac (1/30)) - (endTime - startTime)
        when (diff > 0) $ do
          threadDelay $ floor $ diff * 1000000
        loop
  loop
