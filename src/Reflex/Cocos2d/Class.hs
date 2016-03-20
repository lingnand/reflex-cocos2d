{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Class where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Concurrent
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

class ( ReflexHost t, MonadIO m, MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      , MonadAsyncException m, MonadAsyncException (HostFrame t)) => NodeGraph t m where
    askParent :: m Node
    -- | Run a graph under a given node
    subGraph :: IsNode n => n -> m a -> m a
    -- | Run a graph with the initial content and the updated content
    -- whenever the event updates
    holdGraph :: IsNode n => n -> m a -> Event t (m b) -> m (a, Event t b)
    -- | Schedule an action to occur after the current cohort has been
    -- built; this is necessary because Behaviors built in the current
    -- cohort may not be read until after it is complete
    schedulePostBuild :: HostFrame t () -> m ()
    performEvent_ :: Event t (HostFrame t ()) -> m ()
    performEventMaybe :: NodeGraph t m => Event t (HostFrame t (Maybe a)) -> m (Event t a)
    performEventMaybe e = do
        runWithActions <- askRunWithActions
        (eResult, trigger) <- newEventWithTriggerRef
        performEvent_ . ffor e $ \o -> o >>= \case
                Just result -> void . liftIO . forkIO $ readRef trigger
                                        >>= mapM_ (\t -> runWithActions [t :=> Identity result])
                _ -> return ()
        return eResult
    performEvent :: NodeGraph t m => Event t (HostFrame t a) -> m (Event t a)
    performEvent = performEventMaybe . (fmap Just <$>)
    mapHMaybe :: (a -> HostFrame t (Maybe b)) -> Event t a -> m (Event t b)
    mapHMaybe f = performEventMaybe . (f <$>)
    mapH :: (a -> HostFrame t b) -> Event t a -> m (Event t b)
    mapH f = performEventMaybe . (f' <$>)
        where f' a = Just <$> f a
    mapH_ :: (a -> HostFrame t b) -> Event t a -> m ()
    mapH_ f = performEvent_ . (void . f <$>)
    forHMaybe :: Event t a -> (a -> HostFrame t (Maybe b)) -> m (Event t b)
    forHMaybe = flip mapHMaybe
    forH :: Event t a -> (a -> HostFrame t b) -> m (Event t b)
    forH = flip mapH
    forH_ :: Event t a -> (a -> HostFrame t b) -> m ()
    forH_ = flip mapH_
    -- performEventAsync (we can add it if we need to)
    -- | Return the function that allows to propagate events and execute
    -- the action handlers
    -- NOTE: this should NEVER be run in the main thread
    askRunWithActions :: m ([DSum (EventTrigger t) Identity] -> IO ())

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder |< child@
infixr 2 -<
(-<) :: (NodeGraph t m, IsNode n) => m n -> m a -> m (n, a)
node -< child = do
    n <- node
    a <- subGraph n child
    return (n, a)

-- | Hold
-- e.g., @newChild & nodeBuilder |- child0@
infixr 2 -|
(-|) :: (NodeGraph t m, IsNode n) => m n -> m a -> Event t (m a) -> m (n, Dynamic t a)
(-|) node child0 newChild = do
    n <- node
    (result0, newResult) <- holdGraph n child0 newChild
    dyn <- holdDyn result0 newResult
    return (n, dyn)

-- | View
-- e.g., @nodeBuilder |= child@
infixr 2 =|
(=|) :: (NodeGraph t m, IsNode n) => m n -> Dynamic t (m a) -> m (n, Event t a)
node =| child = do
    n <- node
    (e, trigger) <- newEventWithTriggerRef
    runWithActions <- askRunWithActions
    schedulePostBuild . void . liftIO . forkIO $ readRef trigger >>= mapM_ (\t -> runWithActions [t :=> Identity ()])
    let newChild = leftmost [updated child, tag (current child) e]
    (_, evt) <- holdGraph n (return ()) newChild
    return (n, evt)
