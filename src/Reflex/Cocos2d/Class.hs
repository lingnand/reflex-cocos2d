{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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

class ( Reflex t, MonadHold t m, MonadIO m, MonadAsyncException m, Functor m, MonadReflexCreateTrigger t m
      , MonadIO (GraphHost m), MonadAsyncException (GraphHost m), Functor (GraphHost m), MonadSample t (GraphHost m)
      , MonadFix m ) => NodeGraph t m where
    type GraphHost m :: * -> *
    askParent :: m Node
    subGraph :: Node -> m a -> m a
    -- | Schedule an action to occur after the current cohort has been built; this is necessary because Behaviors built in the current cohort may not be read until after it is complete
    schedulePostBuild :: GraphHost m () -> m ()
    addVoidAction :: Event t (GraphHost m ()) -> m ()
    -- | Return the function that allows to propagate events and execute
    -- the action handlers
    askRunWithActions :: m (ActionTrigger t)
