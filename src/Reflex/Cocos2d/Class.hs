{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Class where

import Data.Dependent.Sum (DSum (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Fix
import Control.Monad.Exception
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

type ActionTrigger t = [DSum (EventTrigger t)] -> IO ()

class ( ReflexHost t, MonadIO m, MonadFix m, MonadHold t m
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      , MonadAsyncException m, MonadAsyncException (HostFrame t)) => NodeGraph t m where
    askParent :: m Node
    subGraph :: Node -> m a -> m a
    -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
    schedulePostBuild :: HostFrame t () -> m ()
    performEvent_ :: Event t (HostFrame t ()) -> m ()
    -- performEvent
    -- performEventAsync (we can add it if we need to)
    -- | Return the function that allows to propagate events and execute
    -- the action handlers
    askRunWithActions :: m (ActionTrigger t)
