module Presentation where


import Slide
import Animation
import Content
import Utils
import GHCJSTime
import ImagePreloader
import Eases
import Image

import Data.Monoid ((<>))

import Life


runPresentation :: IO ()
runPresentation = do
  doc <- getDocument

  ctx <- getContext
  timeRef <- initTime

  setTitle "Genzai: Declarative Slideshows with FRP"
  
  em <- emptyDB
  _ <- updateTime timeRef
  imageDB <- loadImages em [("behaviour", "images/frp-behavior.png"),
                            ("event", "images/frp-event.png"),
                            ("frp-full", "images/frp-example-full.png"),
                            ("haskell", "images/haskell.png"),
                            ("graphics-examples-code", "images/graphics-examples-code.png")
                           ]
  updateTime timeRef >>= (\t -> putStrLn ("Images loaded. Time: " ++ (show t) ++ "ms."))
  
  let font = "22px Garamond"
      fontH1 = "30px Garamond"
      fontH2 = "28px Garamond"
      fontH3 = "26px Garamond"
      fontH4 = "24px Garamond"
  w <- slideWidth
  h <- slideHeight

  slideshow ctx doc imageDB 60 $ do
    slide $ do
      animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ Text "40px Garamond" Nothing "Genzai: Declarative Slideshows using FRP"
      
    -- slide $ do
    --   [_, _, a, _] <- makeBullets "22px Garamond" (100, 100) 40 (Just 600) ctx [
    --     "This is the first line of text. It's not very long at all.",
    --     "This is the second line of text. It's still not that long, but it's a bit longer than the first.",
    --     "This is the third and final line of text. By comparison to the other two lines, it's exceptionally long, potentially rivalling the likes of Tolstoy's War and Peace and the great literary classics of the 19th Century.",
    --     "This line is pretty short." ]
    --   chain [ chainTween tweenTranslate (PairI (200, 200)) easeOutElastic 1000000 ] a
    --   animation [ makeTween tweenTranslate (PairI (400, -300)) (PairI (400, 400)) easeOutBounce 1100000] $ Scale 0.7 0.7 $ Image "yoda" Original

    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Motivation"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 60)) ctx [
        "• Code-based authoring allows for precision and portability",
        "• Traditional presentation software suites have limits to their animation tools",
        "• Existing code-based tools for presentations are very limited"       
        ]
      empty
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Goals for final product"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
        "• Concise, clear syntax",
        "• Compiled presentations can be run anywhere - no dependencies",
        "• Declarative animation system with room for extensions",
        "• Embedded Domain-Specific Language hosted in Haskell"
        ]
      empty
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Functional Reactive Programming"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
        "• Declarative approach to animation and interactive content in a functional style",
        "• Active field of research since 1997",
        "• Haskell used as main research language",
        "• Most graphical FRP frameworks are complicated to install"
        ]
      empty
    slide $ do
      let slEvent = Translate (w %% 30) 200 $ AlignText AlignCenter $
                    (Text fontH3 (Just 400) "Event")
                 <> (Translate 0 50 $ Text font (Just 450) "An event that \"fires\" periodically with a value")
                 <> (Translate 0 250 $ Image "event" Original)
          slBehaviour = Translate (w %% 70) 200 $ AlignText AlignCenter $
                        (Text fontH3 (Just 400) "Behaviour")
                     <> (Translate 0 50 $ Text font (Just 400) "A continuous value that varies with time")
                     <> (Translate 0 250 $ Image "behaviour" Original)
               
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "FRP: Events and Behaviours"
      animation [] slEvent
      animation [] slBehaviour
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "FRP example"
      animation [] $ Translate (w %% 50) (h %% 50) $ Image "frp-full" Original
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "First steps - GHCJS"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
        "• GHCJS - Haskell to JavaScript compiler",
        "• GHCJS works with almost all Haskell libraries",
        "• Foreign Function Interface allows execution of JS code in Haskell"
        ]
      empty
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Graphics"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
        "• Rendering with HTML5 Canvas requires lots of meaningless extra code",
        "• Animations need a clear and consistent interface for rendering",
        "• First major design area; implemented before any other features",
        "• Purely functional interface guarantees consistent rendering",
        " ",
        "• OpenGL-style transformation matrix functions",
        "• Image pre-loader - ensure all images have loaded before rendering them"
        ]
      empty
    slide $ do
      let examplePair = Translate (-(w %% 15)) 0 $ Image "graphics-examples-code" Original <!>
                        Translate (w %% 30) (-(w %% 10)) $ examples
          examples = (Translate 50 120 $ (Image "haskell" Original <>
                                          (Translate 50 0 $ StrokeWidth 4 $ Circle 100) <>
                                          (Rotate 45 $ StrokeWidth 4 $ Rect 150 100))) <>
                     (Circle 100) <>
                     (Translate 200 250 $ (FillColor (RGB 255 0 0) $ 
                                             StrokeColor (RGB 0 255 0) $
                                             StrokeWidth 4 $ FCircle 100))
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Graphics Example"
      animation [] $ Translate (w %% 50) (w %% 25) $ examplePair
    slide $ do
      animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Animation system"
      makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
        "• Works by animating a Tween on a graphics object",
        "• Any property can be tweened - position, rotation, colour, etc.",
        "• Achieve different animation styles using 'Easing functions'"
        ]
      let circle = FCircle 35
      
      animation [] $ Translate (w %% 22) 350 $ Text fontH2 Nothing "Linear"
      animation [ makeTween tweenTranslate (PairI (w %% 50, 350)) (PairI (w %% 80, 350)) easeLinear 2000000 ] $ FillColor red $ circle
      animation [] $ Translate (w %% 22) 430 $ Text fontH2 Nothing "Elastic"
      animation [ makeTween tweenTranslate (PairI (w %% 50, 430)) (PairI (w %% 80, 430)) easeOutElastic 2000000 ] $ FillColor green $ circle
      animation [] $ Translate (w %% 22) 510 $ Text fontH2 Nothing "Bounce"
      animation [ makeTween tweenTranslate (PairI (w %% 50, 510)) (PairI (w %% 80, 510)) easeOutBounce 2000000 ] $ FillColor blue $ circle
    slideGeneric lifeSlide

  