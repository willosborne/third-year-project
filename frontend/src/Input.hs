module Input where

import Data.Word (Word)
import Web.KeyCode (Key)

import GHCJS.DOM.EventM
import GHCJS.DOM.JSFFI.Generated.KeyboardEvent as KeyboardEvent

uiKeyRepeat :: EventM t KeyboardEvent Bool
uiKeyRepeat = event >>= KeyboardEvent.getRepeat

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

  
