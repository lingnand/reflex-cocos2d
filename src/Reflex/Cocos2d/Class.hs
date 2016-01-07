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
    -- | run a graph under the same parent in the HostFrame
    askRunGraph :: m (Node -> m a -> HostFrame t (a, HostFrame t (), Event t (HostFrame t ())))
    -- | run a graph under a given node, return the result and the children
    subGraph :: Node -> m a -> m a
    subGraphWithVoidActions :: Node -> m a -> m (a, Event t (HostFrame t ()))
    -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
    schedulePostBuild :: HostFrame t () -> m ()
    performEvent_ :: Event t (HostFrame t ()) -> m ()
    -- performEvent
    -- performEventAsync (we can add it if we need to)
    -- | Return the function that allows to propagate events and execute
    -- the action handlers
    askRunWithActions :: m (ActionTrigger t)

performEvent :: NodeGraph t m => Event t (HostFrame t a) -> m (Event t a)
performEvent e = do
    runWithActions <- askRunWithActions
    (eResult, trigger) <- newEventWithTriggerRef
    performEvent_ . ffor e $ \o -> do
        result <- o
        liftIO $ readRef trigger >>= mapM_ (\t -> runWithActions [t :=> result])
    return eResult
