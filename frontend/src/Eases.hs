module Eases where

import Animation

easeInQuadratic :: Ease
easeInQuadratic t = t * t

easeInCubic :: Ease
easeInCubic t = t * t * t

easeOutCubic :: Ease
easeOutCubic t = 1 - (easeInCubic (1 - t))

easeInOutCubic :: Ease
easeInOutCubic = inOut easeInCubic easeOutCubic

inOut :: Ease -> Ease -> Ease
inOut easeIn easeOut = \t -> case () of
  _ | t <= 0.5  -> easeIn  (t * 2) / 2
    | otherwise -> easeOut ((t - 0.5)*2) /2 + 0.5

-- easeInOutCubic :: Ease
-- easeInOutCubic t = ((-2) * t + 3) * t * t


easeOutElastic :: Ease
easeOutElastic t = let p = 0.3
                   in (2 ** (-10 * t)) * sin((t - p/4)*(2*pi)/p) + 1
-- easeOutElastic t = let t2 = t * t
--                        t3 = t2 * t
--                        t4 = t3 * t
--                        t5 = t4 * t
--                    in
--                      15  * t
--                    - 67  * t2
--                    + 126 * t3
--                    - 106 * t4
--                    + 33  * t5

easeOutBounce :: Ease
easeOutBounce t
  | t < 1 / 2.75   = a * t * t
  | t < 2 / 2.75   = a * t2 * t2 + 0.75
  | t < 2.5 / 2.75 = a * t3 * t3 + 0.9375
  | otherwise      = a * t4 * t4 + 0.984375
  where
    a = 7.5625
    t2 = t - (1.5 / 2.75)
    t3 = t - (2.25 / 2.75)
    t4 = t - (2.625 / 2.75)

