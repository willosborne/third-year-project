

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


  
