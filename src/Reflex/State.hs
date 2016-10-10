{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.State
    ( AccStateT(AccStateT)
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


import Control.Lens
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.Exception
import Control.Monad.State.Strict
import Reflex
import Reflex.Host.Class
import Data.Maybe

---- generalization: AccStateT
---- the input is always the same - like a reader
newtype AccStateT t f s m a = AccStateT (StateT [Event t (s -> Maybe s)] (ReaderT (f s) m) a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

type DynStateT t = AccStateT t (Dynamic t)
type EventStateT t = AccStateT t (Event t)
type UniqDynStateT t = AccStateT t (UniqDynamic t)
type BehaviorStateT t = AccStateT t (Behavior t)

watch :: Monad m => AccStateT t f s m (f s)
watch = AccStateT . lift $ ask

watches :: (Functor f, Monad m) => (s -> a) -> AccStateT t f s m (f a)
watches f = fmap f <$> watch

{-# INLINE composeMaybe #-}
composeMaybe :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
composeMaybe f g a
  | Just a' <- g a = f a'
  | otherwise      = f a

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

instance MonadTrans (AccStateT t f s) where
    lift = AccStateT . lift . lift

instance MonadReader r m => MonadReader r (AccStateT t f s m) where
    ask = lift ask
    local f m = AccStateT . StateT $ \ts -> ReaderT $ \d -> local f (_runAccStateT m d ts)
    reader = lift . reader

instance MonadSample t m => MonadSample t (AccStateT t f s m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (AccStateT t f s m) where
    hold v0 = lift . hold v0
    holdDyn v0 = lift . holdDyn v0
    holdIncremental v0 = lift . holdIncremental v0

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (AccStateT t f s m) where
    newEventWithTrigger = lift . newEventWithTrigger
    newFanEventWithTrigger = lift . newFanEventWithTrigger

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (AccStateT t f s m) where
    subscribeEvent = lift . subscribeEvent

instance MonadRef m => MonadRef (AccStateT t f s m) where
    type Ref (AccStateT t f s m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r


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
