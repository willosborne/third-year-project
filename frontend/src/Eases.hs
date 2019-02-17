module Eases where

import Animation

easeInQuadratic :: Ease
easeInQuadratic t = t * t

easeInCubic :: Ease
easeInCubic t = t * t * t

easeInOutCubic :: Ease
easeInOutCubic t = ((-2) * t + 3) * t * t


easeOutElastic :: Ease
easeOutElastic t = let t2 = t * t
                       t3 = t2 * t
                       t4 = t3 * t
                       t5 = t4 * t
                   in
                     15  * t
                   - 67  * t2
                   + 126 * t3
                   - 106 * t4
                   + 33  * t5

