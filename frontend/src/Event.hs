module Event where

data Event =
    EventNextSlide
  | EventPreviousSlide
  | EventMouseClick Int Int
  | EventMouseMove Int Int

  
