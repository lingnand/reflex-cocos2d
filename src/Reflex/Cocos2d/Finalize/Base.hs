{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Finalize.Base
    (
      FinalizeT(..)
    , runFinalizeT
    , MonadRun(..)
    )
  where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Exception
import Control.Monad.Ref
import Control.Monad.State
import Reflex
import Reflex.Host.Class

import Reflex.Cocos2d.Finalize.Class
import Reflex.Cocos2d.Builder.Class
import Reflex.Cocos2d.Accum.Class
import Reflex.Cocos2d.FastTriggerEvent.Class

-- | XXX: A fairly contrived class just to make FinalizeT work
class (Monad n, Monad m) => MonadRun n m where
    run :: n () -> m ()

newtype FinalizeT t n m a = FinalizeT { unFinalizeT :: StateT (n ()) m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO
    , MonadException, MonadAsyncException
    )

-- | Run the FinalizeT transformer with some initial finalizer
runFinalizeT :: FinalizeT t n m a -> n () -> m (a, n ())
runFinalizeT = runStateT . unFinalizeT

instance MonadTrans (FinalizeT t n) where
    lift = FinalizeT . lift

instance PerformEvent t m => PerformEvent t (FinalizeT t n m) where
    type Performable (FinalizeT t n m) = Performable m
    {-# INLINABLE performEvent_ #-}
    performEvent_ e = lift $ performEvent_ e
    {-# INLINABLE performEvent #-}
    performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (FinalizeT t n m) where
    {-# INLINABLE getPostBuild #-}
    getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (FinalizeT t n m) where
    {-# INLINABLE newTriggerEvent #-}
    newTriggerEvent = lift newTriggerEvent
    {-# INLINABLE newTriggerEventWithOnComplete #-}
    newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
    {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
    newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FinalizeT t n m) where
    {-# INLINABLE newEventWithTrigger #-}
    newEventWithTrigger = lift . newEventWithTrigger
    {-# INLINABLE newFanEventWithTrigger #-}
    newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadRef m => MonadRef (FinalizeT t n m) where
    type Ref (FinalizeT t n m) = Ref m
    {-# INLINABLE newRef #-}
    newRef = lift . newRef
    {-# INLINABLE readRef #-}
    readRef = lift . readRef
    {-# INLINABLE writeRef #-}
    writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (FinalizeT t n m) where
    {-# INLINABLE atomicModifyRef #-}
    atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (FinalizeT t n m) where
    {-# INLINABLE sample #-}
    sample = lift . sample

instance MonadHold t m => MonadHold t (FinalizeT t n m) where
    {-# INLINABLE hold #-}
    hold v0 v' = lift $ hold v0 v'
    {-# INLINABLE holdDyn #-}
    holdDyn v0 v' = lift $ holdDyn v0 v'
    {-# INLINABLE holdIncremental #-}
    holdIncremental v0 v' = lift $ holdIncremental v0 v'

instance (Monad m, Monad n) => MonadFinalize (FinalizeT t n m) where
    type Finalizable (FinalizeT t n m) = n
    -- NOTE: the later finalizers should be run before the earlier ones
    addFinalizer n = FinalizeT $ modify (n >>)

instance (Monad m, NodeBuilder t m) => NodeBuilder t (FinalizeT t n m) where
    {-# INLINABLE getParent #-}
    getParent = lift getParent
    {-# INLINABLE withParent #-}
    withParent n (FinalizeT m) = FinalizeT $ restoreT . return =<< liftWith (\run -> withParent n (run m))
    {-# INLINABLE getWindowSize #-}
    getWindowSize = lift getWindowSize
    {-# INLINABLE getFrameTicks #-}
    getFrameTicks = lift getFrameTicks

instance FastTriggerEvent t m => FastTriggerEvent t (FinalizeT t n m) where
    fastNewTriggerEvent = lift fastNewTriggerEvent
    fastNewTriggerEventWithOnComplete = lift fastNewTriggerEventWithOnComplete
    fastNewEventWithLazyTriggerWithOnComplete = lift . fastNewEventWithLazyTriggerWithOnComplete

instance (MonadRun n m, MonadAdjust t m, MonadFix m, MonadSample t m, MonadHold t m, MonadSample t n)
        => MonadAdjust t (FinalizeT t n m) where
    runWithReplace zm em = do
        rec ((a, finZ), ers) <- lift $ runWithReplace (runFinalizeT zm (return ())) $ ffor em $ \newM -> do
              -- first run the previous finalizer
              sample finalizerBeh >>= run
              runFinalizeT newM (return ())
            finalizerBeh <- hold finZ (snd <$> ers)
        -- if this block is ever removed, we still need to clean up the latest finalizer
        addFinalizer $ join (sample finalizerBeh)
        return (a, fst <$> ers)
    -- XXX: traverseDMapWithKeyWithAdjust and traverseDMapWithKeyWithAdjustWithMove not implemented
    traverseDMapWithKeyWithAdjust _ _ _ = error "traverseDMapWithKeyWithAdjust not implemented for FinalizeT"
    traverseDMapWithKeyWithAdjustWithMove _ _ _ = error "traverseDMapWithKeyWithAdjustWithMove not implemented for FinalizeT"

instance (MonadHold t m, MonadFix m, MonadAccum t m, MonadSample t n)
        => MonadAccum t (FinalizeT t n m) where
    runWithAccumulation zm em = do
        ((a, finZ), ers) <- lift $
            runWithAccumulation
                (runFinalizeT zm (return ()))
                (flip runFinalizeT (return ()) <$> em)
        let onNewFinalizer old new = new >> old
        finalizerBeh <- accum onNewFinalizer finZ (snd <$> ers)
        addFinalizer $ join (sample finalizerBeh)
        return (a,  fst <$> ers)
