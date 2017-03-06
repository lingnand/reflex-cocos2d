{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.State
    ( AccStateT(..)
    , DynStateT
    , EventStateT
    , UniqDynStateT
    , BehaviorStateT
    , watch
    , watches
    , focus
    , refine
    , runAccStateT
    , _runAccStateT
    , execAccStateT
    , evalAccStateT
    , adjust
    , adjustMaybe
    , composeMaybe
    , pnon
    , pnon'
    )
  where


import Data.Maybe
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.Exception
import Control.Monad.State.Strict
import Reflex
import Reflex.Host.Class

---- generalization: AccStateT
---- the input is always the same - like a reader
newtype AccStateT t f s m a = AccStateT (StateT [Event t (s -> Maybe s)] (ReaderT (f s) m) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO
    , MonadException, MonadAsyncException
    , MonadSample t, MonadHold t
    )

type DynStateT t = AccStateT t (Dynamic t)
type EventStateT t = AccStateT t (Event t)
type UniqDynStateT t = AccStateT t (UniqDynamic t)
type BehaviorStateT t = AccStateT t (Behavior t)

instance MonadTrans (AccStateT t f s) where
    lift = AccStateT . lift . lift

instance MonadTransControl (AccStateT t f s) where
    type StT (AccStateT t f s) a = (a, [Event t (s -> Maybe s)])
    liftWith f = AccStateT . StateT $ \s -> ReaderT $ \r ->
      (\x -> (x, s)) <$> f (\t -> _runAccStateT t r s)
    restoreT mSt = AccStateT . StateT $ \_ -> ReaderT $ const mSt

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (AccStateT t f s m) where
    {-# INLINABLE newEventWithTrigger #-}
    newEventWithTrigger = lift . newEventWithTrigger
    {-# INLINABLE newFanEventWithTrigger #-}
    newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadRef m => MonadRef (AccStateT t f s m) where
    type Ref (AccStateT t f s m) = Ref m
    {-# INLINABLE newRef #-}
    newRef = lift . newRef
    {-# INLINABLE readRef #-}
    readRef = lift . readRef
    {-# INLINABLE writeRef #-}
    writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (AccStateT t f s m) where
    {-# INLINABLE atomicModifyRef #-}
    atomicModifyRef r = lift . atomicModifyRef r

instance PostBuild t m => PostBuild t (AccStateT t f s m) where
    {-# INLINABLE getPostBuild #-}
    getPostBuild = lift getPostBuild

instance PerformEvent t m => PerformEvent t (AccStateT t f s m) where
    type Performable (AccStateT t f s m) = Performable m
    {-# INLINABLE performEvent_ #-}
    performEvent_ e = lift $ performEvent_ e
    {-# INLINABLE performEvent #-}
    performEvent e = lift $ performEvent e

instance TriggerEvent t m => TriggerEvent t (AccStateT t f s m) where
    {-# INLINABLE newTriggerEvent #-}
    newTriggerEvent = lift newTriggerEvent
    {-# INLINABLE newTriggerEventWithOnComplete #-}
    newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
    {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
    newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

watch :: Monad m => AccStateT t f s m (f s)
watch = AccStateT . lift $ ask

watches :: (Functor f, Monad m) => (s -> a) -> AccStateT t f s m (f a)
watches f = fmap f <$> watch

-- | An alternative f <=< g that runs f even if g gives back Nothing
{-# INLINE composeMaybe #-}
composeMaybe :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
composeMaybe g f a
  | Just a' <- f a = g a'
  | otherwise      = g a

runAccStateT :: (Accumulator t f, MonadHold t m, MonadFix m) => AccStateT t f s m a -> s -> m (a, f s)
runAccStateT ms initial = mdo
    stateDyn <- accumMaybe (&) initial $ mergeWith composeMaybe ts
    (a, ts) <- _runAccStateT ms stateDyn []
    return (a, stateDyn)

_runAccStateT :: AccStateT t f s m a -> f s -> [Event t (s -> Maybe s)] -> m (a, [Event t (s -> Maybe s)])
_runAccStateT (AccStateT ms) d ts = flip runReaderT d . flip runStateT ts $ ms

execAccStateT :: (Accumulator t f, MonadHold t m, MonadFix m) => AccStateT t f s m a -> s -> m (f s)
execAccStateT ms initial = snd <$> runAccStateT ms initial

evalAccStateT :: (Accumulator t f, MonadHold t m, MonadFix m) => AccStateT t f s m a -> s -> m a
evalAccStateT ms initial = fst <$> runAccStateT ms initial

focus :: (Reflex t, MonadHold t m, Functor f) => ALens' s a -> AccStateT t f a m b -> AccStateT t f s m b
focus len am = do
    let clonedGetter = cloneLens len
        clonedSetter = cloneLens len
    da <- watches (^.clonedGetter)
    (b, ta') <- lift $ _runAccStateT am da []
    let !lts = clonedSetter <$> mergeWith composeMaybe ta'
    adjustMaybe lts
    return b

refine :: (f2 a -> f1 a) -> AccStateT t f1 a m b -> AccStateT t f2 a m b
refine f m = AccStateT . StateT $ \ts -> ReaderT $ \d -> _runAccStateT m (f d) ts

adjust :: (Reflex t, Monad m) => Event t (s -> s) -> AccStateT t f s m ()
adjust et = let !et' = fmap Just <$> et in AccStateT $ modify (et':)

adjustMaybe :: Monad m => Event t (s -> Maybe s) -> AccStateT t f s m ()
adjustMaybe !et = AccStateT $ modify (et:)

---- Helpers

-- | partial non: useful to convert a Prism to something that takes a default
-- e.g., pnon _Just 20 is similar to non 20
-- NOTE: this isn't an Iso, because the conversion from a to s is impossible when a == def
-- that means e.g., Nothing ^. pnon _Just 3 = 3; review (pnon _Just 3) 3 != Nothing
pnon' :: APrism' s a -> a -> Lens' s a
pnon' p def = lens (fromMaybe def . preview (clonePrism p)) (const (review (clonePrism p)))

-- non without the need for Eq (and doesn't reset source to Nothing when default is supplied)
pnon :: a -> Lens' (Maybe a) a
pnon = pnon' _Just
