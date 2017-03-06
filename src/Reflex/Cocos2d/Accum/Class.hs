{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Accum.Class
    (
      MonadAccum(..)
    )
  where

import Data.Coerce
import Data.Sequence
import Data.Unique.Tag
import Data.Functor.Misc
import Data.Semigroup
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Dependent.Map (DMap)
import Control.Monad.State.Strict
import Reflex
import Reflex.Host.Class
import Reflex.State

-- | Dual of MonadAdjust - instead of running replacement on new events, accumulate the effects
class (Reflex t, Monad m) => MonadAccum t m | m -> t where
    runWithAccumulation :: m a -> Event t (m b) -> m (a, Event t b)

instance (MonadFix m, MonadHold t m, MonadAccum t m) => MonadAccum t (PostBuildT t m) where
    -- similar to the MonadAdjust implementation
    runWithAccumulation z em = do
      postBuild <- getPostBuild
      lift $ do
        rec result@(_, result') <- runWithAccumulation (runPostBuildT z postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') em
            let voidResult' = fmapCheap (const ()) result'
        return result

instance (MonadFix m, MonadHold t m, MonadAccum t m) => MonadAccum t (AccStateT t f s m) where
    runWithAccumulation z em = AccStateT $ StateT $ \s -> ReaderT $ \r -> do
      ((a, sz), ers) <- runWithAccumulation (_runAccStateT z r []) $ (\m -> _runAccStateT m r []) <$> em
      let onNewAdjusters _ [] = Nothing
          onNewAdjusters adj adjs = Just $! mergeWith composeMaybe (adjs++[adj])
      adjBeh <- accumMaybe onNewAdjusters (mergeWith composeMaybe sz) (snd <$> ers)
      return ((a, fst <$> ers), switch adjBeh:s)

-- TODO: MonadAccum implementation for PerformEventT
instance (ReflexHost t, PrimMonad (HostFrame t)) => MonadAccum t (PerformEventT t m) where
    runWithAccumulation outerA0 outerA' = PerformEventT $ runWithAccumulationRequesterTWith f (coerce outerA0) (coerceEvent outerA')
      where f :: HostFrame t a -> Event t (HostFrame t b) -> RequesterT t (HostFrame t) Identity (HostFrame t) (a, Event t b)
            f a0 a' = do
              result0 <- lift a0
              result' <- requestingIdentity a'
              return (result0, result')

-- XXX: duplicated functions to achieve accumulation
runWithAccumulationRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m, MonadFix m)
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithAccumulationRequesterTWith f a0 a' =
  let f' :: forall a' b'. ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m a'
         -> Event t (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m b')
         -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (a', Event t b')
      f' x y = do
        r <- EventWriterT ask
        unRequesterT (f (runReaderT x r) (fmapCheap (`runReaderT` r) y))
  in RequesterT $ runWithAccumulationEventWriterTWith f' (coerce a0) (coerceEvent a')

runWithAccumulationEventWriterTWith :: forall m t w a b. (Reflex t, MonadHold t m, MonadFix m, Semigroup w)
                               => (forall a' b'. m a' -> Event t (m b') -> EventWriterT t w m (a', Event t b'))
                               -> EventWriterT t w m a
                               -> Event t (EventWriterT t w m b)
                               -> EventWriterT t w m (a, Event t b)
runWithAccumulationEventWriterTWith f a0 a' = do
  let g :: EventWriterT t w m c -> m (c, Seq (Event t w))
      g (EventWriterT r) = runStateT r mempty
      combine :: Seq (Event t w) -> Event t w
      combine = fmapCheap sconcat . mergeList . toList
  (result0, result') <- f (g a0) $ fmap g a'
  request <- accum (<>) (combine $ snd result0) $ fmapCheap (combine . snd) result'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  -- TODO: test if coincidence leads to nasty loop because we are
  -- recombining the old event into the new event
  EventWriterT $ modify $ flip (|>) $ coincidence $ updated request
  EventWriterT $ modify $ flip (|>) $ switch $ current request
  return (fst result0, fmapCheap fst result')
