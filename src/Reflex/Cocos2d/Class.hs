{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Class
    ( Time
    , NodeBuilderEnv(NodeBuilderEnv)
    , parent
    , windowSize
    , postBuildEvent
    , frameTicks
    , runWithActions
    , NodeBuilder(..)
    , (-<)
    , (-|)
    , (-<<)
    , (<-<)
    , postponeCurrent
    , buildDyn
    , buildDyn'
    , runDyn
    -- * Free
    , switchFree
    , switchFreeT
    , runGreedyFreeT
    , buildGreedyFreeT
    , waitEvent
    , waitEvent'
    , waitEvent_
    , waitDynMaybe
    , waitDynMaybe'
    , waitDynMaybe_
    -- * Time
    , modulate
    -- * Rand
    , runRandEvent
    )
  where

import Data.Monoid
import Data.Tuple (swap)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Bifunctor
import Diagrams (V2)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Trans.Free
import Control.Monad.Trans.Writer
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Exception
import Control.Monad.Random
import Control.Lens
import Reflex
import Reflex.State
import Reflex.Host.Class
import Graphics.UI.Cocos2d.Node

type Time = Float -- ^ in seconds

data NodeBuilderEnv t = NodeBuilderEnv
    { _parent  :: !Node
    , _windowSize     :: V2 Float
    , _postBuildEvent :: !(Event t ())
    , _frameTicks     :: !(Event t Time) -- ^ Ticks for each frame
    , _runWithActions :: !(([DSum (EventTrigger t) Identity], IO ()) -> IO ())
    }

frameTicks ::
  forall t. Lens' (NodeBuilderEnv t) (Event t Time)
frameTicks
  f
  (NodeBuilderEnv p winSize pb ticks run)
  = fmap
      (\ ticks'
         -> NodeBuilderEnv p winSize pb ticks' run)
      (f ticks)
{-# INLINE frameTicks #-}
parent :: forall t. Lens' (NodeBuilderEnv t) Node
parent
  f
  (NodeBuilderEnv p winSize pb ticks run)
  = fmap
      (\ p'
         -> NodeBuilderEnv p' winSize pb ticks run)
      (f p)
{-# INLINE parent #-}
postBuildEvent ::
  forall t. Lens' (NodeBuilderEnv t) (Event t ())
postBuildEvent
  f
  (NodeBuilderEnv p winSize pb ticks run)
  = fmap
      (\ pb'
         -> NodeBuilderEnv p winSize pb' ticks run)
      (f pb)
{-# INLINE postBuildEvent #-}
runWithActions ::
  forall t.
  Lens' (NodeBuilderEnv t) (([DSum (EventTrigger t) Identity],
                                 IO ())
                                -> IO ())
runWithActions
  f
  (NodeBuilderEnv p winSize pb ticks run)
  = fmap
      (\ run'
         -> NodeBuilderEnv p winSize pb ticks run')
      (f run)
{-# INLINE runWithActions #-}
windowSize ::
  forall t. Lens' (NodeBuilderEnv t) (V2 Float)
windowSize
  f
  (NodeBuilderEnv p winSize pb ticks run)
  = fmap
      (\ winSize'
         -> NodeBuilderEnv p winSize' pb ticks run)
      (f winSize)
{-# INLINE windowSize #-}

class (Reflex t, Monad im, Monad m) => EventSequenceHolder t im m where
    seqHold :: (im a) -> Event t (im a) -> m (Event t a)


class (Reflex t, Monad im, Monad m) => EventSequencer t im m where
    seqEventMaybe :: Event t (im (Maybe a)) -> m (Event t a)
    seqEventMaybe = fmap (fmapMaybe id) . seqEvent
    seqEvent_ :: Event t (im ()) -> m ()
    seqEvent_ = void . seqEvent
    seqEvent :: Event t (im a) -> m (Event t a)
    seqEvent = seqEventMaybe . fmap (Just <$>)

    forEventMaybe :: Event t a -> (a -> im (Maybe b)) -> m (Event t b)
    forEventMaybe e = seqEventMaybe . ffor e
    forEvent :: Event t a -> (a -> im b) -> m (Event t b)
    forEvent e = seqEvent . ffor e
    forEvent_ :: Event t a -> (a -> im ()) -> m ()
    forEvent_ e = seqEvent_ . ffor e

    filterMEvent :: (a -> im Bool) -> Event t a -> m (Event t a)
    filterMEvent f e = seqEventMaybe . ffor e $ \v -> do
                            b <- f v
                            return $ guard b >> return v

class Reflex t => SequenceAccumulator t f | f -> t where
    seqAccum :: (MonadHold t m, MonadFix m, EventSequencer t im m)
             => (a -> b -> im a) -> a -> Event t b -> m (f a)
    seqAccum f = seqAccumMaybe $ \v o -> Just <$> f v o

    seqAccumMaybe :: (MonadHold t m, MonadFix m, EventSequencer t im m)
                  => (a -> b -> im (Maybe a)) -> a -> Event t b -> m (f a)

    seqMapAccum :: (MonadHold t m, MonadFix m, EventSequencer t im m)
                => (a -> b -> im (a, c)) -> a -> Event t b -> m (f a, Event t c)
    seqMapAccum f = seqMapAccumMaybe $ \v o -> bimap Just Just <$> f v o

    seqMapAccumMaybe :: (MonadHold t m, MonadFix m)
                     => (a -> b -> im (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)

seqMapAccum_ :: forall t im m a b c. (Reflex t, MonadHold t m, MonadFix m, EventSequencer t im m)
             => (a -> b -> im (a, c)) -> a -> Event t b -> m (Event t c)
seqMapAccum_ f z e = do
    (_ :: Dynamic t a, result) <- seqMapAccum f z e
    return result

seqMapAccumMaybe_ :: forall t im m a b c. (MonadHold t m, MonadFix m)
                  => (a -> b -> im (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
seqMapAccumMaybe_ f z e = do
    (_ :: Dynamic t a, result) <- seqMapAccumMaybe f z e
    return result

class Reflex t => SequenceAccumulator t (Dynamic t) where
    seqAccumMaybe f z e = do
      rec evt <- seqEvent $ pushAlways e $ \b -> do
            a <- sample (current da)
            f a b
          da <- holdDyn z $ fmapMaybe id evt
      return da
    seqMapAccumMaybe f z e = do
      rec evt <- seqEvent $ pushAlways e $ \b -> do
            a <- sample (current da)
            f a b
          da <- holdDyn z $ fmayMaybe fst evt
      return (da, fmapMaybe snd evt)

class (MonadIO m, Reflex t, MonadHold t m, MonadFix m, EventSequencer t im m)
     => EventSequencer t (RandT StdGen im) m where
    seqEventMaybe rands = do
      g <- liftIO newStdGen
      seqMapAccumMaybe (\g ma -> first Just . swap <$> runRandT ma g) g rands

-- Add Finalizers that will be run on each new event

class (EventSequencer t im m, MonadFix m, MonadHold t m)
     => EventSequencer t (FinalizerT im) m where
    seqEventMaybe e = seqMapAccumMaybe_ f (return ()) e
      where f lastFin (FinalizerT m) = lastFin >> first Just . swap <$> runWriterT m

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 -<
(-<) :: (NodeBuilder t m, NodePtr n) => m n -> m a -> m (n, a)
(-<) node child = do
    n <- node
    a <- local (parent .~ n) child
    return (n, a)

-- | Hold
-- e.g., @newChild & nodeBuilder -| child0@
infixr 2 -|
(-<<) :: NodeBuilder t m => m a -> Event t (m a) -> m (Dynamic t a)
(-<<) child0 newChild = do
    (result0, newResult) <- holdNodes child0 newChild
    dyn <- holdDyn result0 newResult
    return (n, dyn)

-- | View
-- e.g., @nodeBuilder -< child@
dynNodes :: NodeBuilder t m => Dynamic t (m a) -> m (Event t a)
dynNodes child = do
    (_, evt) <- holdNodes (return ()) =<< postponeCurrent child
    return evt

-- | Greedy Free
freeNodes :: (NodeBuilder t m, NodePtr n) => FreeT (Event t) m a -> m (Event t a)
freeNodes ft = mdo
    pe <- view postBuildEvent
    let startE = case result0 of
          Pure a -> return a <$ pe
          Free e -> e
    newFs <- switchPromptly startE $ fmapMaybe previewFree newResult
    (result0, newResult) <- holdNodes (runFreeT ft) (runFreeT <$> newFs)
    return $ fmapMaybe previewPure newResult

-- | Convert to an Event that carries the first value in postBuild
postponeCurrent :: NodeBuilder t m => Dynamic t a -> m (Event t a)
postponeCurrent d = do
    pe <- view postBuildEvent
    return $ leftmost [ pushAlways (const $ sample (current d)) pe
                      , updated d ]

-- lazy version of building dyn
accumDynNodes :: NodeBuilder t m => Dynamic t (m a) -> m (Event t a)
accumDynNodes = buildEvent <=< postponeCurrent

-- strict version
accumDynNodes' :: NodeBuilder t m => Dynamic t (m a) -> m (Dynamic t a)
accumDynNodes' d = do
    a <- join $ sample (current d)
    evt <- buildEvent (updated d)
    holdDyn a evt

seqDyn :: (NodeBuilder t m, EventSequencer t im m) => Dynamic t (im a) -> m (Event t a)
seqDyn = seqEvent <=< postponeCurrent


switchFreeT' :: (Reflex t, MonadHold t m)
          => (forall x. m' x -> m x)
          -> (forall x. m' x -> PushM t x)
          -> FreeT (Event t) m' a -> m (FreeF (Event t) a a)
switchFreeT' hoistM hoistPush ft = hoistM (runFreeT ft) >>= \case
    Pure a -> return $ Pure a
    Free e -> Free <$> switchPromptly never flattened
      where flattened = flip pushAlways e $ switchFreeT' hoistPush hoistPush >=> \case
                            Pure a -> return $ a <$ e
                            Free ie -> return ie

-- | Merge a deeply nested Event into a single Event
switchFree :: NodeBuilder t m => Free (Event t) a -> m (Event t a)
switchFree f = do
    let hoist (Identity x) = return x
    switchFreeT' hoist hoist f >>= \case
      Pure a -> fmap (a <$) $ view postBuildEvent
      Free e -> return e

switchFreeT :: NodeBuilder t m => FreeT (Event t) (PushM t) a -> m (Event t a)
switchFreeT f = do
    e <- view postBuildEvent
    let e' = flip pushAlways e $ \_ -> switchFreeT' id id f >>= \case
          Pure a -> return $ a <$ e
          Free e -> return e
    switchPromptly never e'

-- | Depth visit the nested events in the FreeT
seqFreeNodes :: (NodeBuilder t m, EventSequencer t im m) => FreeT (Event t) im a -> m (Event t a)
seqFreeNodes ft = do
    pe <- view postBuildEvent
    rec newFs <- switchPromptly (ft <$ pe) $ fmapMaybe previewFree e'
        e' <- seqEvent $ runFreeT <$> newFs
    return $ fmapMaybe previewPure e'

accumFreeNodes :: NodeBuilder t m => FreeT (Event t) m a -> m (Event t a)
accumFreeNodes ft = runFreeT ft >>= \case
    Pure v -> fmap (v <$) $ view postBuildEvent
    Free startE -> mdo
      newFs <- switchPromptly startE $ fmapMaybe previewFree e'
      e' <- accumNodes $ runFreeT <$> newFs
      return $ fmapMaybe previewPure e'

-- | Wait for the first occurrence
waitEvent :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m a
waitEvent = liftF

-- | Wait for the first occurrence and include the future occurrences in return
waitEvent' :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m (a, Event t a)
waitEvent' e = (,e) <$> liftF e

waitEvent_ :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m ()
waitEvent_ = void . waitEvent

-- | Wait for the Dynamic to turn from Nothing to Just
waitDynMaybe :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m a
waitDynMaybe dyn = lift (sample $ current dyn) >>= \case
    Just a -> return a
    _ -> waitEvent $ fmapMaybe id (updated dyn)

-- | Wait for the first Just value, and include the future values in return
waitDynMaybe' :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m (a, Event t a)
waitDynMaybe' dyn = (,fmapMaybe id $ updated dyn) <$> waitDynMaybe dyn

waitDynMaybe_ :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m ()
waitDynMaybe_ = void . waitDynMaybe

previewPure :: FreeF f a b -> Maybe a
previewPure (Pure a) = Just a
previewPure _ = Nothing

previewFree :: FreeF f a b -> Maybe (f b)
previewFree (Free fb) = Just fb
previewFree _ = Nothing


-- | Locally modulate the ticks in the environment
modulate :: (Reflex t, MonadHold t m, MonadFix m, Num a, Ord a) => a -> Event t a -> m (Event t a)
modulate limit = mapAccumMaybe_ f (0, limit)
    where
      f (acc, l) d = let sum = acc + d in
        if sum > l then (Just (0  , limit-(sum-l)) , Just sum)
                   else (Just (sum, l            ) , Nothing )

-- implement NodeBuilder instance so that we don't need to keep lifting...
instance NodeBuilder t m => NodeBuilder t (AccStateT t f s m) where
    -- askRunWithActionsAsync = lift askRunWithActionsAsync
    subNode n m = AccStateT . StateT $ \ts -> ReaderT $ \d -> subNode n (_runAccStateT m d ts)
    holdNodes ma emb = do
      d <- watch
      ((a, tsZ), erb) <- lift $ holdNodes (_runAccStateT ma d []) $ ffor emb $ \mb -> _runAccStateT mb d []
      let et = (mergeWith composeMaybe . snd) <$> erb
          tz = mergeWith composeMaybe tsZ
      switchPromptly tz et >>= adjustMaybe
      return (a, fst <$> erb)
    buildEvent em = mdo
      d <- watch
      built <- lift . buildEvent $ flip pushAlways em $ \m -> do
                    t <- sample behT
                    return $ _runAccStateT m d [t]
      let et = (mergeWith composeMaybe . snd) <$> built
      behT :: Behavior t (Event t (s -> Maybe s)) <- hold (never :: Event t (s -> Maybe s)) et
      adjustMaybe $ switch behT
      return $ fst <$> built
    buildEvent_ = void . buildEvent
    seqEventMaybe = lift . seqEventMaybe
    seqEvent_ = lift . seqEvent_
    seqEvent = lift . seqEvent
