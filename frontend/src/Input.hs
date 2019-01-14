module Input where

import Data.Word (Word)
import Web.KeyCode (Key)

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
  | EventMouseDown Int Int MouseButton
  | EventMouseMove Int Int
  | EventKeyDown Key
  | EventKeyUp Key
  deriving (Show, Eq)

  
