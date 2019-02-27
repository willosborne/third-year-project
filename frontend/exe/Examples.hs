-- Local Variables:
-- eval: (git-gutter+-mode -1)
-- End:

keyPressed :: Event Key

lastKeyPressed :: Behaviour Key
lastKeyPressed <- hold None keyPressed

appendString :: Key -> String -> String

lastKeyPressed :: Behaviour String
typedString <- accumulateB "" (appendString <$> keyPressed)

counter :: Behaviour Int
counter <- accumulateB 0 (+1 $> keyPressed)

  


circle :: Content
circle = Circle 100

transforms :: Content
transforms = Image "haskell.png" <>
             Translate 50 0 (circle) <>
             Rotate 45 (Rectangle 150 100)

colours :: Content
colours = FillColor (RGB 255 0 0)
          (StrokeColor (RGB 0 255 0)
            (FCircle 100))

  
rectangle :: Content
rectangle = FilledRectangle 35 35

anim1 :: Animation
anim1 <- animation [
  makeTween tweenPosition  (Position (150, 380)) (Position (500, 380)) easeSin    1.5,
  makeTween tweenFillColor (Color black)         (Color red)           easeLinear 1.5
  ] rectangle
  
anim2 :: Animation
anim2 <- chain [
  chainTween tweenPosition  (Position (500, 600)) easeBounce 1.0,
  chainTween tweenFillColor (Color blue)          easeBounce 1.0
  ] anim1


slide $ do
    animation [] $
      Translate (50%) 100 $
      AlignText AlignCenter $
      Text fontTitle "Slide system"
    makeBullets font (20%, 200) [
      "• Simple syntax; almost no code clutter",
      "• Use animation and chain functions to add animations to slide",
      "• Standard Haskell code: can write your own functions easily"
    ]


