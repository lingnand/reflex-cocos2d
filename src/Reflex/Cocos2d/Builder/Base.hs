{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Cocos2d.Builder.Base
    (
      NodeBuilderEnv(..)
    , ImmediateNodeBuilderT(..)
    , runImmediateNodeBuilderT
    )
  where

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Base
import Data.Dependent.Sum
import Debug.Trace
import Diagrams (V2)

import Graphics.UI.Cocos2d.Director
import Graphics.UI.Cocos2d.Node
import Reflex
import Reflex.Host.Class

import Reflex.Cocos2d.Accum.Class
import Reflex.Cocos2d.Builder.Class
import Reflex.Cocos2d.FastTriggerEvent.Class

-- implementation via
data NodeBuilderEnv t = NodeBuilderEnv
    { parent      :: !Node
    , windowSize  :: !(V2 Float)
    , frameTicks  :: !(Event t Time)
    -- different from FireCommand in that it's already lifted into IO
    , fireEvent :: !([DSum (EventTrigger t) Identity] -> IO ())
    }

newtype ImmediateNodeBuilderT t m a = ImmediateNodeBuilderT { unImmediateNodeBuilderT :: ReaderT (NodeBuilderEnv t) m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO
    , MonadException, MonadAsyncException
    , MonadSample t, MonadHold t
    )

instance (MonadBase b m) => MonadBase b (ImmediateNodeBuilderT t m) where
    liftBase = lift . liftBase

instance MonadTrans (ImmediateNodeBuilderT t) where
    lift = ImmediateNodeBuilderT . lift

instance (Reflex t, Monad m) => NodeBuilder t (ImmediateNodeBuilderT t m) where
    {-# INLINABLE getParent #-}
    getParent = ImmediateNodeBuilderT $ asks parent
    {-# INLINABLE withParent #-}
    withParent p' = ImmediateNodeBuilderT . local (\env -> env { parent = p' }) . unImmediateNodeBuilderT
    {-# INLINABLE getWindowSize #-}
    getWindowSize = ImmediateNodeBuilderT $ asks windowSize
    {-# INLINABLE getFrameTicks #-}
    getFrameTicks = ImmediateNodeBuilderT $ asks frameTicks

instance PostBuild t m => PostBuild t (ImmediateNodeBuilderT t m) where
    {-# INLINABLE getPostBuild #-}
    getPostBuild = lift getPostBuild

instance PerformEvent t m => PerformEvent t (ImmediateNodeBuilderT t m) where
    type Performable (ImmediateNodeBuilderT t m) = Performable m
    {-# INLINABLE performEvent_ #-}
    performEvent_ e = lift $ performEvent_ e
    {-# INLINABLE performEvent #-}
    performEvent e = lift $ performEvent e

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ImmediateNodeBuilderT t m) where
    {-# INLINABLE newEventWithTrigger #-}
    newEventWithTrigger = lift . newEventWithTrigger
    {-# INLINABLE newFanEventWithTrigger #-}
    newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadRef m => MonadRef (ImmediateNodeBuilderT t m) where
    type Ref (ImmediateNodeBuilderT t m) = Ref m
    {-# INLINABLE newRef #-}
    newRef = lift . newRef
    {-# INLINABLE readRef #-}
    readRef = lift . readRef
    {-# INLINABLE writeRef #-}
    writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ImmediateNodeBuilderT t m) where
    {-# INLINABLE atomicModifyRef #-}
    atomicModifyRef r = lift . atomicModifyRef r

instance (Ref m ~ Ref IO, MonadRef m, MonadReflexCreateTrigger t m)
  => TriggerEvent t (ImmediateNodeBuilderT t m) where
    {-# INLINABLE newTriggerEvent #-}
    newTriggerEvent = do
        (e, t) <- newTriggerEventWithOnComplete
        return (e, \a -> t a $ return ())
    {-# INLINABLE newTriggerEventWithOnComplete #-}
    newTriggerEventWithOnComplete = do
        fire <- ImmediateNodeBuilderT $ asks fireEvent
        (eResult, reResultTrigger) <- newEventWithTriggerRef
        return . (,) eResult $ \a cb -> do
          scheduler <- director_getInstance >>= director_getScheduler
          -- NOTE: we have to make sure the triggering is performed in the main thread
          scheduler_performFunctionInCocosThread scheduler $ do
            me <- readRef reResultTrigger
            forM_ (trace "Triggering event posted to cocos thread" me) $ \t -> fire [t ==> a]
            cb
    {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
    newEventWithLazyTriggerWithOnComplete f = do
        fire <- ImmediateNodeBuilderT $ asks fireEvent
        newEventWithTrigger $ \t ->
          f $ \a cb -> do
            scheduler <- director_getInstance >>= director_getScheduler
            -- NOTE: we have to make sure the triggering is performed in the main thread
            scheduler_performFunctionInCocosThread scheduler $ do
              fire $ trace "Triggering event posted to cocos thread" [t ==> a]
              cb

instance (Ref m ~ Ref IO, MonadRef m, MonadReflexCreateTrigger t m)
  => FastTriggerEvent t (ImmediateNodeBuilderT t m) where
    {-# INLINABLE fastNewTriggerEvent #-}
    fastNewTriggerEvent = do
        (e, t) <- fastNewTriggerEventWithOnComplete
        return (e, \a -> t a $ return ())
    {-# INLINABLE fastNewTriggerEventWithOnComplete #-}
    fastNewTriggerEventWithOnComplete = do
        fire <- ImmediateNodeBuilderT $ asks fireEvent
        (eResult, reResultTrigger) <- newEventWithTriggerRef
        return . (,) eResult $ \a cb -> do
          readRef reResultTrigger >>= mapM_ (\t -> fire [t ==> a])
          cb
    {-# INLINABLE fastNewEventWithLazyTriggerWithOnComplete #-}
    fastNewEventWithLazyTriggerWithOnComplete f = do
        fire <- ImmediateNodeBuilderT $ asks fireEvent
        newEventWithTrigger $ \t ->
          f $ \a cb -> do
            fire [t ==> a]
            cb

instance MonadAdjust t m => MonadAdjust t (ImmediateNodeBuilderT t m) where
    runWithReplace (ImmediateNodeBuilderT a0) a' = ImmediateNodeBuilderT $ runWithReplace a0 (unImmediateNodeBuilderT <$> a')
    traverseDMapWithKeyWithAdjust f dm0 dm' =
        ImmediateNodeBuilderT $ traverseDMapWithKeyWithAdjust (\k v -> unImmediateNodeBuilderT $ f k v) dm0 dm'
    traverseDMapWithKeyWithAdjustWithMove f dm0 dm' =
        ImmediateNodeBuilderT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unImmediateNodeBuilderT $ f k v) dm0 dm'

instance MonadAccum t m => MonadAccum t (ImmediateNodeBuilderT t m) where
    runWithAccumulation zm em = ImmediateNodeBuilderT $ ReaderT $ \r ->
        runWithAccumulation (runReaderT (unImmediateNodeBuilderT zm) r)
          ((\m -> runReaderT (unImmediateNodeBuilderT m) r) <$> em)

runImmediateNodeBuilderT :: ImmediateNodeBuilderT t m a -> NodeBuilderEnv t -> m a
runImmediateNodeBuilderT = runReaderT . unImmediateNodeBuilderT
