

keyPressed :: Event Key

lastKeyPressed :: Behaviour Key
lastKeyPressed <- hold None keyPressed

appendString :: Key -> String -> String

lastKeyPressed :: Behaviour String
typedString <- accumulateB "" (appendString <$> keyPressed)

counter :: Behaviour Int
counter <- accumulateB 0 (+1 $> keyPressed)

  
