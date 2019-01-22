module Example where


{-
Notes

slideshow could build up slideshow using WriterT of slides
slides do the same with objects and events


Are slide objects Events or behaviours

In order to render each frame, we need behaviours for sampling
Need an event that changes state of objects

Sample objects then fold to one Content to render with <>

could use <@> to apply a time-varying function to a Content

The Content itself will not change; it will just have transforms applied to it


could use switch to select the currently running animation

current animation stored with an accumB, function switches to next animation in an internal list
we then render the current state with switchB

how to store a running animation? - also, how to `activate' a future animation
an animation could be a time value, an easing function, a rendering function (could encapsulate start and end values) and a total animation time
also need to store start and end values

could also have an Easable type class to define lerps over more complex stuff such as colors

-}

slideshow $ do
  slide $ do
    rect <- translated $ Rect 100 100
