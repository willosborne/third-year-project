module Presentation where

import Slide
import Animation
import Content
import Utils
import GHCJSTime
import ImagePreloader
import Eases
import Image
import Render (setDefaultFillColor)

import Data.Monoid ((<>))

import Life

backgroundColor :: Color
backgroundColor = RGB 40 40 40

runPresentation :: IO ()
runPresentation = do
  -- setup code
  doc <- getDocument
  ctx <- getContext
  timeRef <- initTime

  setTitle "Genzai: Declarative Slideshows with FRP"
  
  -- initialise image preloader
  em <- emptyDB
  _ <- updateTime timeRef
  imageDB <- loadImages em [("behaviour", "images/frp-behavior.png"),
                            ("event", "images/frp-event.png"),
                            ("frp-full", "images/frp-example-full.png"),
                            ("frp-minimal", "images/frp-example-withoutstring.png"),
                            ("haskell", "images/haskell.png"),
                            ("graphics-examples-code", "images/graphics-examples-code.png"),
                            ("animation-chaining-code", "images/animation-chain-code.png"),
                            ("slide-example-code", "images/slide-example-large.png"),
                            ("gantt-chart", "images/project-gantt.png")
                           ]
  updateTime timeRef >>= (\t -> putStrLn ("Images loaded. Time: " ++ (show t) ++ "ms."))
  
  let font = "22px Garamond"
      fontH1 = "30px Garamond"
      fontH2 = "28px Garamond"
      fontH3 = "26px Garamond"
      fontH4 = "24px Garamond"
      fontTitle = "40px Garamond"
      fontSectionTitle = "38px Garamond"
  w <- slideWidth
  h <- slideHeight

  setDefaultFillColor ctx white

  -- start slideshow
  slideshow ctx doc imageDB 60 $ do
    -- slide $ do
    --   animation [ fix (pairI Translate) (PairI (w %% 30, h %% 50)) ] $ FillColor black $ Text font (Just 500) "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
    -- slide $ do
    --   animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ StrokeWidth 3 $ Translate 100 100 (Rotate 45 (RegularPolygon 3 100))
      
    -- slide $ do
    --   animation [ fix (pairI Translate) (PairI (w %% 40, h %% 50)) ] $ FillColor red $ StrokeWidth 3 $ FCircle 100
    --   animation [ fix (pairI Translate) (PairI (w %% 60, h %% 50)) ] $ FillColor black $ StrokeWidth 3 $ FCircle 100

    -- slide $ do
    --   animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ FillColor red $ StrokeWidth 3 $FCircle 100
    -- set background colour for main presentation
    background backgroundColor $ do
      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ (Text fontTitle Nothing "Genzai: Declarative Slideshows using FRP") <> (Translate 0 70 $ Text fontH1 Nothing "Will Osborne")
        
      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Motivation"
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Motivation"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 60)) ctx [
          "• Code-based authoring allows for precision - we can get precisely the results we want",
          "• Results are portable, requiring no specific software to view",
          "• Traditional presentation software suites have limits to their animation tools",
          "• Existing code-based tools for presentations are limited"       
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
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Functional Reactive Programming"
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Functional Reactive Programming"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Declarative approach to animation and interactive content in a functional style",
          "• Active field of research since 1997",
          "• A variety of different approaches, each with their own pros and cons",
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
      slide' backgroundColor $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "FRP example"
        animation [] $ Translate (w %% 50) (h %% 50) $ Image "frp-minimal" Original
      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 50)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Design and Development"
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "First steps - GHCJS"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• GHCJS - Haskell to JavaScript compiler",
          "• GHCJS works with the entire standard library, and almost all external libraries",
          "• Foreign Function Interface allows execution of JS code in Haskell"
          ]
        empty
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Graphics"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• First major design area; implemented before any other features",

          "• Rendering with HTML5 Canvas requires lots of meaningless extra code",
          "• Animations need a clear and consistent interface for rendering",
          
          "• Purely functional \"Content\" interface guarantees consistent rendering",
          "• OpenGL-style transformation matrix functions",
          
          "<spacer>",
          
          "• Image pre-loader - ensure all images have loaded before rendering them"
          ]
        empty
      slide $ do
        let examplePair = Translate (-(w %% 15)) 0 $ Image "graphics-examples-code" Original <!>
                          Translate (w %% 30) (-(w %% 10)) $ examples
            examples = (Translate 50 120 $ (Image "haskell" Original <>
                                            (Translate 50 0 $ StrokeWidth 4 $ Circle 100) <>
                                            (Rotate 45 $ StrokeWidth 4 $ Rect 150 100))) <>
                       (StrokeWidth 4 $ Circle 100) <>
                       (Translate 200 250 $ (FillColor (RGB 255 0 0) $ 
                                               StrokeColor (RGB 0 255 0) $
                                               StrokeWidth 4 $ FCircle 100))
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Graphics Example"
        animation [] $ Translate (w %% 50) (w %% 30) $ examplePair
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Animation system: Tweens"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Animate a property of an object by 'tweening' it between two values.",
          -- "• A tween takes a visual property of an object, and animates it between two values",
          "• Any property can be tweened - position, rotation, colour, etc.",
          "• Purely functional interface ensures consistent results",
          "• Achieve different animation styles using 'Easing functions'"
          ]
        let circle = FCircle 35
            top = 400
        
        animation [] $ Translate (w %% 22) top $ Text fontH2 Nothing "Linear"
        animation [ makeTween tweenTranslate (PairI (w %% 50, top)) (PairI (w %% 80, top)) easeLinear 1500000 ] $ FillColor red circle
        animation [] $ Translate (w %% 22) (top + 80) $ Text fontH2 Nothing "Elastic"
        animation [ makeTween tweenTranslate (PairI (w %% 50, top + 80)) (PairI (w %% 80, top + 80)) easeOutElastic 1500000 ] $ FillColor amber circle
        animation [] $ Translate (w %% 22) (top + 160) $ Text fontH2 Nothing "Bounce"
        animation [ makeTween tweenTranslate (PairI (w %% 50, top + 160)) (PairI (w %% 80, top + 160)) easeOutBounce 1500000 ] $ FillColor green circle

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Animation system: Chaining"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Animations can be chained together - each starts where the previous one ended",
          "• If you can animate it, you can chain it",
          "• Implementation of this feature required significant design decisions"
          ]
        animation [] $ Translate (w %% 50) (h %% 40) $ Scale 0.9 0.9 $ Image "animation-chaining-code" Original
  
        let rect = FRect 70 70
        
        a1 <- animation [ makeTween tweenTranslate (PairI (w %% 70, 200)) (PairI (w %% 85, 200)) easeInOutCubic 1500000
                        , makeTween tweenFillColor (ColorI black) (ColorI red) easeLinear 1500000 ] rect
        chain [ chainTween tweenTranslate (PairI (w %% 85, h %% 90)) easeOutBounce 1500000
              , chainTween tweenFillColor (ColorI blue) easeOutBounce 1500000 ] a1
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "FRP framework"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Based on Sodium framework",
          "• Simple and easy to use compared to most FRP libraries",
          "• Can be used for anything: not just slideshows!"
          ]
        empty
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Slide system"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Concept: A slide is a list of animations and static content",
          "• Simple syntax; almost no code clutter",
          "• Use animation and chain functions to add animations to slide",
          "• Standard Haskell code: can add your own functionality easily"
          ]
        animation [] $ Translate (w %% 50) 350 $ Image "slide-example-code" Original
      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Generic Slideshow Interface"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• General interface for displaying on-screen content",
          "• The slide/animation system implements this for ease of use",
          "• Any function that implements the interface can be used instead!",
          "• Example: Conway's Game of Life"
          ]
        empty


      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 40)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Demo"
        animation [ makeTween tweenTranslate (PairI (w %% 50, -50)) (PairI (w %% 50, h %% 60)) easeOutBounce 1700000 ] $ AlignText AlignCenter $ Text fontH1 Nothing "You're watching it now!"

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Genzai In Action"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Entire slideshow constructed in Genzai",
          "• About 280 lines of code including all text, animations and content",
          "• Live rendering of all shapes and animations",
          "• Embedding interactive applications: Conway's Game of Life"
          ]
        empty
  
      slideGeneric lifeSlide

      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 40)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Project Management"

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Project Management"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• One major feature at a time, with research alongside",
          "• Weekly meetings with project supervisor",
          "• Use of Git for version control: shared Github repository"
          ]
        animation [] $ Translate (w %% 50) 350 $ Image "gantt-chart" Original

      slide $ do
        animation [ fix (pairI Translate) (PairI (w %% 50, h %% 40)) ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Evaluation"

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Contributions and Technical Achievements"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Achieved goal of simple, declarative syntax",
          "• Zero dependencies to run presentations - highly portable",
          "• Most powerful code-based presentation tool I have found",
          "• Seamless integration of interactive content and presentations is a unique feature"
          ]
        empty

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Lessons learnt"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Development of large projects in Haskell is very productive",
          "• Sometimes it is easier to reinvent the wheel when libraries are undocumented",
          "• Academic Haskell papers present a simplified view; internally much more complex"
          ]
        empty

      slide $ do
        animation [] $ Translate (w %% 50) 100 $ AlignText AlignCenter $ Text fontH1 Nothing "Future work"
        makeBullets font (w %% 20, 200) 40 (Just (w %% 50)) ctx [
          "• Polish and release as official library",
          "• Improving FRP system - optimisation and formal guarantees of performance",
          "• Integration with JavaScript libraries - e.g. D3.js for powerful graphing",
          "• Scaling presentations to look correct on every type of screen"
          ]
        empty

      slide $ do
        animation [ makeTween tweenTranslate (PairI (w %% 50, -50)) (PairI (w %% 50, h %% 50)) easeOutElastic 1700000 ] $ AlignText AlignCenter $ Text fontSectionTitle Nothing "Questions"
