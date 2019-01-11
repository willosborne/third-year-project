module Input where

import Data.Word (Word)

data MouseButton = ButtonLeft | ButtonRight | ButtonMiddle deriving (Eq, Show)

toMouseButton :: Word -> Maybe MouseButton
toMouseButton w = case w of
  0 -> Just ButtonLeft
  1 -> Just ButtonMiddle
  2 -> Just ButtonRight
  _ -> Nothing

data UserEvent =
    EventNextSlide
  | EventPreviousSlide
  | EventMouseClick Int Int MouseButton
  | EventMouseMove Int Int
  | EventKeyDown Int
  | EventKeyUp Int
  deriving (Show, Eq)

  
