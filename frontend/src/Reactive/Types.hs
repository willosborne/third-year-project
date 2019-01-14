module Reactive.Types where


type Time = Double

-- type Signal a = Time -> a
-- type SF a b = Signal a -> Signal b

type Behaviour a = Time -> a

instance Functor Behaviour where
  -- fmap :: (a -> b) -> Behaviour a -> Behaviour b
  fmap f b = f . b


type Event a = [(Time, a)]

instance Functor Event where
  fmap f e = map (\(t, a) -> (t, f a)) e


instance Functor Behaviour where
  fmap f b = \t -> f $ b t

instance Applicative Behaviour where
  pure a = \_ -> a
  (<*>) :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
  f <*> b1 = \t -> (f t) (f b1)

filterE :: (a -> Bool) -> Event a -> Event a
filterE f e = filter (f . snd) e

union :: Event a -> Event a -> Event a
union [] ((ty, y):ys) = (ty, y): union [] ys
union ((tx, x):xs) [] = (tx, x) : union xs []
union ((tx, x):xs) ((ty, y):ys)
  | tx <= ty = (tx, x) : union xs ((ty, y) : ys)
  | tx >  ty = (ty, y) : union ((tx, x) : xs) ys

-- stepped :: Event a -> Time -> a
-- stepped es t = 

apply :: Behaviour (a -> b) -> Event a -> Event b
apply behavior es = [(t, (behaviour t) val) | (t, val) <- es]

stepper :: a -> Event a -> Behaviour a
stepper x0 es = \t -> case takeWhile (\(t', _) -> t' < t) es of
                        [] -> x0 -- none < t, return default
                        xs -> snd (last xs) -- otherwise return the last of the ones < t i.e. most recent

filterJust :: Event (Maybe a) -> Event a
filterJust [] = []
filterJust ((t, Just x):xs) = (t, x) : filterJust xs
filterJust ((t, Nothing):xs) = filterJust xs

filterMapJust :: (a -> Maybe b) -> Event a -> Event b
filterMapJust f :: filterJust . fmap f

