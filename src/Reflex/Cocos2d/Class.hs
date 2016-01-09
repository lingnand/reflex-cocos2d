{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Class where

import Data.Dependent.Sum (DSum (..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Lens
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

type ActionTrigger t = [DSum (EventTrigger t)] -> IO ()

class ( ReflexHost t, MonadIO m, MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      , MonadAsyncException m, MonadAsyncException (HostFrame t)) => NodeGraph t m where
    askParent :: m Node
    -- | Run a graph under the same parent in the HostFrame
    askRunGraph :: m (Node -> m a -> HostFrame t (a, HostFrame t (), Event t (HostFrame t ())))
    -- | Run a graph under a given node, return the result and the children
    subGraph :: Node -> m a -> m a
    subGraphWithVoidActions :: Node -> m a -> m (a, Event t (HostFrame t ()))
    -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
    schedulePostBuild :: HostFrame t () -> m ()
    performEvent_ :: Event t (HostFrame t ()) -> m ()
    performEventMaybe :: NodeGraph t m => Event t (HostFrame t (Maybe a)) -> m (Event t a)
    performEventMaybe e = do
        runWithActions <- askRunWithActions
        (eResult, trigger) <- newEventWithTriggerRef
        performEvent_ . ffor e $ \o -> o >>= \case
                Just result -> liftIO $ readRef trigger
                                        >>= mapM_ (\t -> runWithActions [t :=> result])
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
    askRunWithActions :: m (ActionTrigger t)

