{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Cocos2d.FastTriggerEvent.Class
    (
      FastTriggerEvent(..)
    )
  where

import Reflex
import Control.Monad.Trans

import Reflex.State

-- | This class provides 'fast' triggering counterparts on top of TriggerEvent class
-- In the context of Cocos, this usually mean that the trigger functions can be used as long as they
-- are run in the *main* thread (which is the same as the UI thread)
class Monad m => FastTriggerEvent t m | m -> t where
    fastNewTriggerEvent :: m (Event t a, a -> IO ())
    fastNewTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ())
    fastNewEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

instance FastTriggerEvent t m => FastTriggerEvent t (PostBuildT t m) where
    fastNewTriggerEvent = lift fastNewTriggerEvent
    fastNewTriggerEventWithOnComplete = lift fastNewTriggerEventWithOnComplete
    fastNewEventWithLazyTriggerWithOnComplete = lift . fastNewEventWithLazyTriggerWithOnComplete

instance FastTriggerEvent t m => FastTriggerEvent t (AccStateT t f s m) where
    fastNewTriggerEvent = lift fastNewTriggerEvent
    fastNewTriggerEventWithOnComplete = lift fastNewTriggerEventWithOnComplete
    fastNewEventWithLazyTriggerWithOnComplete = lift . fastNewEventWithLazyTriggerWithOnComplete
