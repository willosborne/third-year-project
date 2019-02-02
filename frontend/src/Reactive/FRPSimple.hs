{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
module Reactive.FRPSimple where

-- http://travis.athougies.net/posts/2015-05-05-frp-made-simple.html
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Writer hiding (listen)
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Unique
import Data.Monoid
import Data.IORef

import System.Mem.Weak -- oh god, weak pointers

type React = IO
-- Moment now builds up a list of IOs inside it -- subcomputations built up with >>=
newtype Moment a = Moment { runMoment :: StateT (S.Set Unique) (WriterT ([IO ()], [IO ()]) IO) a }
  deriving ( Monad,
             Applicative,
             Functor,
             MonadWriter ([IO ()], [IO ()]),
             MonadState (S.Set Unique),
             MonadIO,
             MonadFix )
                                       

-- take a register function and return an unregister 
type RegisterEventListener a = (a -> Moment ()) -> IO (IO ())
newtype Event a = Event { eventRegisterListener :: RegisterEventListener a }
-- registerListener :: (a -> Moment ()) -> IO (IO ())


instance Monoid (Event a) where
  mempty = never
  mappend = merge

never :: Event a
never = Event (\_ -> return (return ()))

-- |Merge two events. The result will fire whenever either of the supplied events do.
merge :: Event a -> Event a -> Event a
merge a b = Event (\listener -> do
                      -- take listener for output event
                      -- pass it into the register listener for each of the other events
                      -- combine the two unregister events and return it
                      unregA <- eventRegisterListener a listener
                      unregB <- eventRegisterListener b listener
                      return (unregA >> unregB))

instance Functor Event where
  fmap f ea = Event { eventRegisterListener = (\listener -> eventRegisterListener ea (listener . f)) } -- any events pass through f before listener

data Behaviour a = Behaviour
                 { behaviourUpdates :: Event () -- fires whenever behaviour updates
                 , behaviourGetValue :: Moment a } -- function to get current value

instance Applicative Behaviour where
  -- no updates, just return a every time
  pure a = Behaviour { behaviourUpdates = mempty,
                       behaviourGetValue = return a }

  -- update: merge the two
  -- get value: extract function and value and return one applied to the other
  bab <*> ba = Behaviour { behaviourUpdates = behaviourUpdates bab <> behaviourUpdates ba
                         , behaviourGetValue = do
                             ab <- behaviourGetValue bab
                             a <- behaviourGetValue ba
                             return (ab a) }

instance Functor Behaviour where
  fmap f b = pure f <*> b

-- |Create a new Event and return it, along with a function to fire its listeners with a supplied value.
newEvent :: React (Event a, a -> Moment ())
newEvent = do
  (registerListener, propagateListeners) <- newEventRegistration
  return (Event registerListener, propagateListeners)

-- return a pair - a register function, and a function to pass the param through all attached listeners
newEventRegistration :: React (RegisterEventListener a, a -> Moment ())
newEventRegistration = do
  listeners <- newIORef M.empty
  -- when called, registerListener adds a callback to the listeners map
  let registerListener listener = do
        listenerKey <- newUnique -- guaranteed to return a unique key every time this is called
        -- for each listener, insert it into the map with its unique key
        modifyIORef listeners (M.insert listenerKey listener)
        -- return new map 
        return (modifyIORef listeners (M.delete listenerKey))
      -- propagateListeners fires the event on all listening callbacks.
      propagateListeners x = do
        listeners' <- M.elems <$> liftIO (readIORef listeners)
        mapM_ ($ x) listeners'
  -- return functions - map is stored in closure IORef
  return (registerListener, propagateListeners)

-- |Takes an initial value and an Event, and produces a stepper Behaviour which updates its current value whenever the Event fires.
hold :: a -> Event a -> React (Behaviour a)
hold initial updates = do
  cell <- newIORef initial
  let behaviour = Behaviour { behaviourUpdates = () <$ updates -- turn an Event a into an Event (); this registers handlers to mimic updates
                            , behaviourGetValue = liftIO (readIORef cell) }
  -- register listener. instead of directly writing, we add the write operation to the enclosed list of actions
  unregisterUpdates <- eventRegisterListener updates (\x -> tell ([writeIORef cell x], []))
  -- addFinalizer behaviour unregisterUpdates -- use weak pointers to run the IO action (unregisterUpdates) when behaviour is cleaned up
  return behaviour

-- run a Moment inside IO
sync :: Moment a -> IO a
sync m = do
  (a, (updateHolds, afterHolds)) <- runWriterT (evalStateT (runMoment m) S.empty)
  -- execute all the updates
  sequence_ updateHolds
  sequence_ afterHolds
  return a


listen :: Event a -> (a -> Moment ()) -> IO (IO ())
listen = eventRegisterListener


-- NOTE: querying an Event is just fine, but not a behaviour. Check out this funciton.
listenToBehaviour :: Behaviour a -> (a -> Moment ()) -> IO (IO ())
listenToBehaviour b handle = do
  sync $ do
    initial <- sample b
    handle initial
  -- listen (behaviourUpdates b) (\() -> let handle' = sync (sample b >>= handle)
  --                                     in tell ([],[handle']))
  let handle' = sync (sample b >>= handle)
  listen (behaviourUpdates b) (\() -> tell ([], [handle']))

sample :: Behaviour a -> Moment a
sample = behaviourGetValue

accumE :: a -> Event (a -> a) -> React (Event a)
accumE initial updaters = do
  cell <- newIORef initial
  (registerListener, propagateListeners) <- newEventRegistration
  let event = Event registerListener
  unregisterEventListener <- eventRegisterListener updaters $ \updater -> do
    -- liftIO (modifyIORef cell updater)
    -- cellValue <- liftIO (readIORef cell)
    -- propagateListeners cellValue
    cellValue <- liftIO (modifyIORef cell updater >> readIORef cell)
    propagateListeners cellValue
    
  -- NOTE: this is being called earlier than it should. The problem relates to GHCJS and weak pointers.
  -- addFinalizer event $ putStrLn "Event GC'd!" >> unregisterEventListener
  return event

accumB :: a -> Event (a -> a) -> React (Behaviour a)
accumB initial updaters = do
  accEvt <- accumE initial updaters
  hold initial accEvt

-- can subscribe to a spilled list event; this then fire the event for every item in the list
-- NB produces simultaneous events
spill :: Event [a] -> Event a
spill eas = Event (\listener -> eventRegisterListener eas $
                    \as -> mapM_ listener as)

-- takes an event that fires multiple times within a Moment
-- modifies it to fire only once
calm :: Event a -> React (Event a)
calm event = do
  key <- liftIO newUnique -- new key for this event
  -- make a new event and give it a key for identification
  let evtOut = Event $ \listener -> eventRegisterListener event (calmed listener)
      calmed listener a = do
        -- get set of uniques and check if key is already a member; insert it if so and fire
        alreadyCalled <- S.member key <$> get 
        when (not alreadyCalled) $ do
          modify (S.insert key)
          listener a
  return evtOut


-- takes a Behaviour (Event a) - this has a current event which may change
-- produces an event which fires whenever the *current* event fires
switchE :: Behaviour (Event a) -> Event a
switchE be = Event $ \listener -> do
  eInitial <- sync $ sample be -- get initial current event
  unregisterV <- newIORef (return ()) -- value storing unregister action of current event
  
  unregisterListener <- eventRegisterListener eInitial listener -- unregister action of current event
  writeIORef unregisterV unregisterListener -- store it in unregisterV

  let switchToNewEvent = do
        unregisterFromOld <- readIORef unregisterV
        unregisterFromOld -- unregister from old event

        -- now subscribe to new event
        eNext <- sync $ sample be
        unregisterNewListener <- eventRegisterListener eNext listener -- get new unreg action
        writeIORef unregisterV unregisterNewListener
  
  -- register switchToNewEvent to the after section of the behaviour's update event
  -- save the unregister action for use in this event's unregister
  unregisterBehaviourListener <- eventRegisterListener (behaviourUpdates be) $
                                 \() -> tell ([], [switchToNewEvent])

  return $ do
    -- unregister everything
    unregisterEvtListener <- readIORef unregisterV
    unregisterEvtListener
    unregisterBehaviourListener

-- similar, but it switches between Behaviours instead of Events.
switchB :: Behaviour (Behaviour a) -> Behaviour a
switchB bb = Behaviour { behaviourUpdates = switchE (behaviourUpdates <$> bb)
                       , behaviourGetValue = do
                           b <- sample bb -- get the current behaviour and then sample it
                           sample b }


-- |Takes a behaviour, and produce an event that fires every time the behaviour changes
updates :: Behaviour a -> React (Event a)
updates b = do
  (registerListener, propagateListeners) <- newEventRegistration -- make a new event
  -- this takes the current value of the behaviour and propagates it to listeners of our new event
  let propagate = sync $ sample b >>= propagateListeners

  -- register an action - simply add propagate to the after updates list with tell.
  -- store the unregister for later
  unregisterAction <- eventRegisterListener (behaviourUpdates b) $ \() -> tell ([], [propagate])

  -- disabled till i figure out the finalizer thing
  -- addFinalizer (behaviourUpdates ba) unregisterAction

  return $ Event registerListener


-- |Apply current function in behaviour to values supplied to event
apply :: Behaviour (a -> b) -> Event a -> Event b
apply bab ea = Event (\listener -> eventRegisterListener ea $ \x -> do
                         f <- sample bab
                         (listener . f) x)
                    
-- |Synonym for 'apply'
(<@>) :: Behaviour (a -> b) -> Event a -> Event b
(<@>) = apply

-- |When the Event fires, its value is replaced with the current value of the Behaviour
(<@) :: Behaviour a -> Event b -> Event a
ba <@ eb = Event (\listener -> eventRegisterListener eb $
                   \_ -> (sample ba >>= listener))

-- |Fire an event only when the behaviour evaluates True
whenE :: Behaviour Bool -> Event a -> Event a
whenE bbool ea = Event (\listener -> eventRegisterListener ea $ \x -> do
                           firep <- sample bbool
                           when firep $ listener x)
  

-- |Filter an event to only fire with values that satisfy the predicate
filterE :: (a -> Bool) -> Event a -> Event a
filterE p ea = Event (\listener -> eventRegisterListener ea $ \x ->
                         when (p x) $ listener x)
                       
-- filterBList :: (a -> Bool) -> [Behaviour a] -> [Behaviour a]
