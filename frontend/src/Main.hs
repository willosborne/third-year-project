module Main where

-- import GHCJS
import GHCJS.DOM
import GHCJS.DOM.Window (getInnerWidth, getInnerHeight)
-- import GHCJS.DOM.Node
import GHCJS.DOM.Types hiding (Text)
import GHCJS.DOM.Document
import GHCJS.DOM.Element
-- import GHCJS.DOM.EventM
-- import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.HTMLCanvasElement (getContext)
-- import GHCJS.DOM.CanvasRenderingContext2D
-- import GHCJS.DOM.CanvasPath
import GHCJS.DOM.NonElementParentNode (getElementById)
-- import Control.Monad.IO.Class (MonadIO(..))

-- import Web.KeyCode (Key(..))

import Data.Monoid ((<>))
-- import Data.IORef

import Unsafe.Coerce (unsafeCoerce)
import qualified JavaScript.Web.Canvas

-- import Data.JSString.Internal.Type (JSString)

import Content
-- import Render
import Image
import ImagePreloader
import Animation
import GHCJSTime
-- import Input
import Slide

import Life
-- import Control.Monad.Reader
-- import Control.Concurrent
import Control.Monad.Writer hiding (fix)
-- import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)

run :: a -> a
run = id

main :: IO ()
main = run helloMain

-- from shine
toContext :: Element -> IO CanvasRenderingContext2D
toContext c = do 
  Just ctx <- getContext (unsafeCoerce c) "2d" ["2d"] -- NOTE: extremely hacky way of passing in Element as a JSVal
  return $ unsafeCoerce ctx 

-- canvasStyle :: String
-- canvasStyle = "\"border:1px \
--               \solid #00000; \
--               \top:0px;bottom:0px;left:0px;right:0px;\""

canvasContext :: Document -> String -> IO CanvasRenderingContext2D
canvasContext doc attributes = do
  Just body <- getBody doc
  setInnerHTML body ("<canvas id=\"canvas\" " ++ attributes ++ " </canvas> ")
  Just canvas <- getElementById doc "canvas"
  -- canvas <- elementSetClientWidth w
  -- canvas <- htmlCanvasElementSetHeight h
  toContext canvas

-- | Create a fixed size canvas given the dimensions
fixedSizeCanvas :: Document -> Int -> Int -> IO CanvasRenderingContext2D
fixedSizeCanvas doc x y = canvasContext doc $ attributes x y
  where attributes :: Int -> Int -> String
        attributes x' y' = "width=\""++ show x' ++ "\" \
                           \height=\""++ show y' ++ "\" \
                           \style=\"border:1px \
                           \solid #000000;\
                           \padding: 0px 0px 0px 0px;\""

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO JavaScript.Web.Canvas.Context
--  -- | Create a full screen canvas
-- fullScreenCanvas :: Document -> IO CanvasRenderingContext2D
-- fullScreenCanvas doc = canvasContext doc attributes
--   where attributes :: String
--         attributes = "style=\"border:1px \
--                      \solid #000000; \
--                      \top:0px;bottom:0px;left:0px;right:0px;\""                          

cross :: Content
cross = Translate (-75) (-100) $ FPolygon [(0, 0), (50, 0), (50, 50), (100, 50), (100, 100), (50, 100), (50, 200), (0, 200), (0, 100), (-50, 100), (-50, 100), (-50, 50), (0, 50)]

drawing :: Content
drawing = Translate 200 200 $
  (Image "egg" Original) <>
  (FillColor (RGB 255 0 0) $ StrokeWidth 3 $ Rotate 180 $ Scale 3 3 cross) <> -- filled, stroked path
  (FillColor (RGB 0 255 0) $ FRegularPolygon 8 100) <> -- filled, stroked regular polygon
  (StrokeColor (RGB 0 0 255) $ StrokeWidth 4 $ Path [(-50, -50), (50, -50), (-50, 50)]) <> -- coloured path
  (Translate 0 100 $ Text "22px Garamond" (Just 600) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec placerat laoreet vestibulum. Nam pellentesque libero a urna bibendum, at pellentesque libero tincidunt. Mauris elementum neque et lacus sollicitudin blandit. Mauris ut felis sodales, viverra dui vitae, bibendum est. Sed iaculis mauris eget orci maximus rutrum ac quis urna. Aenean fermentum semper sapien vel aliquam. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Cras consectetur at eros eget tempor. Ut et ligula suscipit, cursus metus et, rutrum tortor. Praesent iaculis efficitur arcu, vitae venenatis neque convallis ac. Nulla at risus purus. Vestibulum sit amet enim condimentum, facilisis nunc ac, iaculis nunc.") <>
  (FillColor (RGBA 0 255 255 0.5) $ Translate 0 150 $ FCircle 50) <>
  (Translate 300 0 $ Image "yoda" Original) 


testTranslate :: Double -> Content -> Content
testTranslate x c = Translate x 0 c

ball :: Content
ball = (FillColor (RGB 255 0 0) $ StrokeWidth 2 $ FCircle 100)
    <> (Translate 0 100 $ Image "egg" Original)

test :: Writer [String] ()
test = do
  tell ["yeet"]
  return ()

testWrite :: [String]
testWrite = execWriter $ do
  test
  test
  test

      
helloMain :: IO ()
helloMain = do
  win <- currentWindowUnchecked
  doc <- currentDocumentUnchecked
  -- body <- getBodyUnchecked doc

  timeRef <- initTime

  putStrLn $ show $ testWrite

  -- forkIO $ do
  --   threadDelay 5000000
  --   putStrLn "Yeet delayed"

  -- doc :: (IsEventTarget t) => t
  -- click :: (IsEventTarget t, IsEvent e) EventName t e
  -- action is EventM t e ()
  -- returns IO (IO ()) - this removes the listener from the element
  -- make a listener

  -- EventM e t a = ReaderT (t, e) IO a
  -- _ <- on doc click $ do
  --   (x, y) <- mouseClientXY
  --   liftIO $ putStrLn $ "Click: " ++ show (x, y)
  --   newPara <- uncheckedCastTo HTMLParagraphElement <$> createElement doc "p"
  --   text <- createTextNode doc $ "Click " ++ show (x, y)
  --   appendChild_ newPara text
  --   appendChild_ body newPara

  w <- getInnerWidth win
  h <- getInnerHeight win
  ctx <- fixedSizeCanvas doc w h

  -- ctx' <- getCtx
  
  -- TODO add quick function to create filled map in one go
  -- maybe >> into flip
  em <- emptyDB
  putStrLn "Stalling till images have loaded..."
  _ <- updateTime timeRef
  imageDB <- loadImages em [("egg", "egg.jpg"), ("yoda", "https://upload.wikimedia.org/wikipedia/en/9/9b/Yoda_Empire_Strikes_Back.png")]
  updateTime timeRef >>= (\t -> putStrLn ("Loaded. Time: " ++ (show t) ++ "ms."))
  

  -- runReaderT (render ctx drawing) imageDB

  -- animate ctx imageDB $ \t -> return $ Translate (t* 100) 200 $ Scale ((sin t)) ((sin (t * 2)) * 0.5 + 1) $ Image "yoda" Original 

  -- simple runProgram example with (time, speed) as the state
  -- let initialState = (0.0, 1.0, 0.0)
  --     renderState (t, _, angle) = let k = (sin (t * 1.5)) + 1
  --                     in
  --                       return $ Translate 600 300 $ Scale k k $ Rotate angle $ FillColor (RGB 255 0 0) $ Image "yoda" Original
  --     processEvent ev (t, speed, angle) = do
  --       case ev of
  --         EventMouseDown _ _ ButtonLeft -> return (t, speed + 0.1, angle)
  --         EventMouseDown _ _ ButtonRight -> return (t, speed - 0.1, angle)
  --         EventKeyDown ArrowRight -> return (t, speed, angle + 10.0)
  --         EventKeyDown ArrowLeft -> return (t, speed, angle - 10.0)
  --         _ -> return (t, speed, angle)
  --     processTime t (_, speed, angle) = return (t * speed, speed, angle)
      
  -- runProgram ctx doc imageDB initialState renderState processEvent processTime

  -- animateReact ctx doc imageDB

  -- runReaderT (render ctx ((Image "egg" Original) <> (Translate 100 0 (Image "yoda" Original)))) imageDB

  -- runLife ctx doc
  animA1 <- makeAnimationTagged [
        makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
        , makeTween (tween (pairI Scale)) (PairI (1, 1)) (PairI (2, 2)) easeSin 2000000
        , makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 2000000
        ] $ FRect 100 100
  animA2 <- chainAnimationsTagged [
        chainTween (tween (pairI Translate)) (PairI (400, 100)) easeSin 2000000
        , chainTween (tween (doubleI StrokeWidth)) (DoubleI 10) easeSin 2000000
        ] animA1
  animA3 <- chainAnimationsTagged [chainTween (tween (pairI Translate)) (PairI (800, 300)) easeSin 2000000] animA2
  animA4 <- chainAnimationsTagged [chainTween (tween (pairI Translate)) (PairI (300, 500)) easeSin 2000000] animA3

  animB1 <- makeAnimationTagged [
        makeTween (tween (pairI Translate)) (PairI (400, 100)) (PairI (100, 500)) easeSin 2000000
        , makeTween (tween (pairI Scale)) (PairI (1, 1)) (PairI (1.5, 2)) easeSin 2000000
        , makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 0 255)) easeSin 2000000
        -- , makeTween (tween (doubleI StrokeWidth)) (DoubleI 0) (DoubleI 10) easeSin 2000000
        ] $ FCircle 100
  animB2 <- chainAnimationsTagged [chainTween (tween (pairI Translate)) (PairI (400, 300)) easeSin 2000000] animB1
  animB3 <- chainAnimationsTagged [chainTween (tween (pairI Translate)) (PairI (200, 500)) easeSin 2000000,
                                   chainTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) easeSin 2000000] animB2
  animB4 <- chainAnimationsTagged [chainTween (tween (pairI Translate)) (PairI (800, 100)) easeSin 2000000] animB3
  yoda1 <- makeAnimationTagged [makeTween (tween (doubleI Rotate)) (DoubleI 0) (DoubleI 270) easeSin 3000000] $ Image "yoda" Original
  yoda2 <- chainAnimationsTagged [chainTween (tween (doubleI Rotate)) (DoubleI 0) easeSin 2000000] yoda1

  let anims = [animA1, animB1, yoda1, animA2, animB2, animA3, animB3, yoda2, animA4, animB4]

  -- inputs <- generateInputs doc 60

  -- (s1Loop, s1Prev, s1Next) <- slide anims ctx doc imageDB inputs 60 

  slideshow ctx doc imageDB 60 $ do
    slideW anims
    tell [lifeSlide]
    slideWW $ do
      c <- makeAnimationW [ makeTween (tween (pairI Translate)) (PairI (100, 100)) (PairI (600, 500)) easeSin 2000000
                          , fix (colorI FillColor) (ColorI (RGB 0 0 255)) ] $ FCircle 100
      r <- makeAnimationW [ makeTween (tween (colorI FillColor)) (ColorI (RGB 255 0 0)) (ColorI (RGB 0 255 0)) easeSin 1000000] $ FRect 100 100
      chainAnimationW [ chainTween (tween (pairI Translate)) (PairI (400, 300)) easeSin 2000000] c
      chainAnimationW [ chainTween (tween (pairI Translate)) (PairI (800, 600)) easeSin 2000000] r
