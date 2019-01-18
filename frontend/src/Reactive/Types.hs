module Reactive.Types where

-- names: Genzai (present), Miseru (show)

type Time = Double

type Event a = [(Time, a)]

-- instance Functor Event where
--   fmap f e = map (\(t, a) -> (t, f a)) e

type Behaviour a = Time -> a

-- instance Functor Behaviour where
--   fmap f b = \t -> f $ b t

-- instance Applicative Behaviour where
--   pure a = \_ -> a
--   (<*>) :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
--   f <*> b1 = \t -> (f t) (f b1)

-- filterE :: (a -> Bool) -> Event a -> Event a
-- filterE f e = filter (f . snd) e

-- union :: Event a -> Event a -> Event a
-- union [] ((ty, y):ys) = (ty, y): union [] ys
-- union ((tx, x):xs) [] = (tx, x) : union xs []
-- union ((tx, x):xs) ((ty, y):ys)
--   | tx <= ty = (tx, x) : union xs ((ty, y) : ys)
--   | tx >  ty = (ty, y) : union ((tx, x) : xs) ys

-- -- stepped :: Event a -> Time -> a
-- -- stepped es t = 

-- apply :: Behaviour (a -> b) -> Event a -> Event b
-- apply behavior es = [(t, (behaviour t) val) | (t, val) <- es]

-- stepper :: a -> Event a -> Behaviour a
-- stepper x0 es = \t -> case takeWhile (\(t', _) -> t' < t) es of
--                         [] -> x0 -- none < t, return default
--                         xs -> snd (last xs) -- otherwise return the last of the ones < t i.e. most recent

-- filterJust :: Event (Maybe a) -> Event a
-- filterJust [] = []
-- filterJust ((t, Just x):xs) = (t, x) : filterJust xs
-- filterJust ((t, Nothing):xs) = filterJust xs

-- filterMapJust :: (a -> Maybe b) -> Event a -> Event b
-- filterMapJust f = filterJust . fmap f

-- type Moment a = Time -> a
-- instance Functor Moment where
--   fmap f moment = \t -> f $ moment t

-- -- banana's accumE:
-- -- MonadMoment m => a -> Event (a -> a) -> m (Event a)
-- -- does this calculation and wraps it in trimE
-- -- trimE takes another `start' value and just returns all the events > start
-- -- where does this start value come from?
-- -- signature is an m (Event a) - so m must be a funciton of Time
-- accumE :: a -> Event (a -> a) -> Event a
-- accumE _ [] = []
-- accumE x [(t, f) : es] = (t, x') : accumE x' es
--   where
--     x' = f x
  
-- accumB :: a -> Event (a -> a) -> Behaviour a
-- accumB x es = stepper a $ accumE x es

-- accumB :: a -> Event (a -> a) -> Behaviour a
