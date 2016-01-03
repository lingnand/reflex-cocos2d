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

class ( Reflex t, MonadHold t m, MonadIO m, MonadAsyncException m, Functor m
      , MonadAsyncException (HostFrame t), Functor (HostFrame t)
      , MonadFix m, MonadReflexAction t m ) => NodeGraph t m where
    askParent :: m Node
    subGraph :: Node -> m a -> m a
    -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
    schedulePostBuild :: HostFrame t () -> m ()

class (ReflexHost t, Monad m) => MonadReflexAction t m | m -> t where
    addVoidAction :: Event t (HostFrame t ()) -> m ()
    -- | Return the function that allows to propagate events and execute
    -- the action handlers
    askRunWithActions :: m (ActionTrigger t)

class (MonadReflexHost t m, MonadIO m, MonadReflexAction t m) => MonadCocos2dHost t m where

instance (MonadReflexHost t m, MonadIO m, MonadReflexAction t m) => MonadCocos2dHost t m where
