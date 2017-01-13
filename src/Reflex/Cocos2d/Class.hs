{-# LANGUAGE TypeFamilies #-}
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

    , MonadSequenceEvent(..)
    , MonadSequenceHold(..)
    , seqDyn

    , SequenceAccumulator(..)
    , seqMapAccum_
    , seqMapAccumMaybe_

    , NodeBuilder

    , (-<)
    , postponeCurrent
    , postpone

    , seqHoldFree
    , waitEvent
    , waitEvent'
    , waitEvent_
    , waitDynMaybe
    , waitDynMaybe'
    , waitDynMaybe_
    , switchFreeT'
    , switchFree

    , evt
    , dyn
    , uDyn
    , dyn'
    , uDyn'
    )
  where

import Data.Tuple (swap)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Bifunctor
import Diagrams (V2)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Trans.Free
import Control.Monad.Reader
import Control.Lens
import Reflex
import Reflex.Host.Class
import Reflex.State
import Graphics.UI.Cocos2d.Node
import Reflex.Cocos2d.Attributes

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


class (Reflex t, MonadHold t m) => MonadSequenceHold t m where
    type Finalizable m :: * -> *
    -- | run the initial z and on each new computation run the finalizer
    -- accumulated in the last
    seqHold :: (m a) -> Event t (m b) -> m (a, Event t b)
    seqHoldDyn :: (m a) -> Event t (m a) -> m (Dynamic t a)
    seqHoldDyn init e = seqHold init e >>= uncurry holdDyn
    -- strict evaluation
    seqDyn' :: Dynamic t (m a) -> m (Dynamic t a)
    seqDyn' d = sample (current d) >>= flip seqHoldDyn (updated d)
    -- add finalizer into the builder
    addFinalizer :: Finalizable m () -> m ()

-- non-strict evaluation of dynamic
seqDyn :: (MonadSequenceHold t m, MonadReader (NodeBuilderEnv t) m)
       => Dynamic t (m a) -> m (Event t a)
seqDyn d = do
    (_, e) <- seqHold (return ()) =<< postponeCurrent d
    return e

class ( Reflex t, Monad m
      , Functor (Sequenceable m) )
     => MonadSequenceEvent t m | m -> t where
    type Sequenceable m :: * -> *
    seqEventMaybe :: Event t (Sequenceable m (Maybe a)) -> m (Event t a)
    seqEventMaybe = fmap (fmapMaybe id) . seqEvent
    seqEvent :: Event t (Sequenceable m a) -> m (Event t a)
    seqEvent = seqEventMaybe . fmap (Just <$>)
    seqEvent_ :: Event t (Sequenceable m ()) -> m ()
    seqEvent_ = void . seqEvent

    seqMapEventMaybe :: (a -> Sequenceable m (Maybe b)) -> Event t a -> m (Event t b)
    seqMapEventMaybe f = seqEventMaybe . fmap f
    seqMapEvent :: (a -> Sequenceable m b) -> Event t a -> m (Event t b)
    seqMapEvent f = seqEvent . fmap f
    seqMapEvent_ ::  (a -> Sequenceable m ()) -> Event t a -> m ()
    seqMapEvent_ f = seqEvent_ . fmap f

    seqForEventMaybe :: Event t a -> (a -> Sequenceable m (Maybe b)) -> m (Event t b)
    seqForEventMaybe e = seqEventMaybe . ffor e
    seqForEvent :: Event t a -> (a -> Sequenceable m b) -> m (Event t b)
    seqForEvent e = seqEvent . ffor e
    seqForEvent_ :: Event t a -> (a -> Sequenceable m ()) -> m ()
    seqForEvent_ e = seqEvent_ . ffor e

-- instance {-# INCOHERENT #-}
--   (Reflex t, Monad m) => MonadSequenceEvent t Identity m where
--     seqEventMaybe = return . fmapMaybe runIdentity

class Reflex t => SequenceAccumulator t f | f -> t where
    seqAccum :: (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
             => (a -> b -> Sequenceable m a) -> a -> Event t b -> m (f a)
    seqAccum f = seqAccumMaybe $ \v o -> Just <$> f v o

    seqAccumMaybe :: (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
                  => (a -> b -> Sequenceable m (Maybe a)) -> a -> Event t b -> m (f a)

    seqMapAccum :: (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
                => (a -> b -> Sequenceable m (a, c)) -> a -> Event t b -> m (f a, Event t c)
    seqMapAccum f = seqMapAccumMaybe $ \v o -> bimap Just Just <$> f v o

    seqMapAccumMaybe :: (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
                     => (a -> b -> Sequenceable m (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)

seqMapAccum_ :: forall t m a b c. (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
             => (a -> b -> Sequenceable m (a, c)) -> a -> Event t b -> m (Event t c)
seqMapAccum_ f z e = do
    (_ :: Dynamic t a, result) <- seqMapAccum f z e
    return result

seqMapAccumMaybe_ :: forall t m a b c.
  (MonadHold t m, MonadFix m, MonadSequenceEvent t m)
  => (a -> b -> Sequenceable m (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
seqMapAccumMaybe_ f z e = do
    (_ :: Dynamic t a, result) <- seqMapAccumMaybe f z e
    return result

instance Reflex t => SequenceAccumulator t (Dynamic t) where
    seqAccumMaybe f z e = do
      rec evt <- seqEvent $ flip pushAlways e $ \b -> do
            a <- sample (current da)
            return $ f a b
          da <- holdDyn z $ fmapMaybe id evt
      return da
    seqMapAccumMaybe f z e = do
      rec evt <- seqEvent $ flip pushAlways e $ \b -> do
            a <- sample (current da)
            return $ f a b
          da <- holdDyn z $ fmapMaybe fst evt
      return (da, fmapMaybe snd evt)

class
  ( Reflex t
  , MonadSequenceEvent t m
  , MonadSequenceHold t m
  , MonadReader (NodeBuilderEnv t) m
  , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
  , MonadSample t m, MonadHold t m, MonadFix m
  , MonadRef m, Ref m ~ Ref IO
  , MonadIO m

  , MonadReflexCreateTrigger t (Sequenceable m)
  , MonadSubscribeEvent t (Sequenceable m)
  , MonadSample t (Sequenceable m), MonadHold t (Sequenceable m)
  , MonadIO (Sequenceable m), MonadFix (Sequenceable m)
  , MonadRef (Sequenceable m), Ref (Sequenceable m) ~ Ref IO

  , MonadIO (Finalizable m)
  ) => NodeBuilder t m | m -> t where

-- Helper
-- | Convert Dynamic to an Event that carries the first value in postBuild
postponeCurrent :: (Reflex t, MonadReader (NodeBuilderEnv t) m)
                => Dynamic t a -> m (Event t a)
postponeCurrent d = do
    pe <- view postBuildEvent
    return $ leftmost [ pushAlways (const $ sample (current d)) pe
                      , updated d ]

postpone :: (Reflex t, MonadReader (NodeBuilderEnv t) m) => a -> m (Event t a)
postpone v = fmap (const v) <$> view postBuildEvent

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 -<
(-<) :: (NodePtr n, MonadReader (NodeBuilderEnv t) m)
     => m n -> m a -> m (n, a)
(-<) node child = do
    n <- node
    a <- local (parent .~ toNode n) child
    return (n, a)

-- -- | Greedy Free
seqHoldFree :: forall t m a.
               ( MonadFix m
               , MonadReader (NodeBuilderEnv t) m
               , MonadSequenceHold t m )
            => FreeT (Event t) m a -> m (Event t a)
seqHoldFree ft = mdo
    let startE = case result0 of
          Pure _ -> never :: Event t (FreeT (Event t) m a)
          Free e -> e
    newFs <- switchPromptly startE $ fmapMaybe previewFree newResult
    (result0, newResult) <- seqHold (runFreeT ft) (runFreeT <$> newFs)
    case result0 of
      Pure a -> postpone a
      _ -> return $ fmapMaybe previewPure newResult

-- ** Operations to be used in seqHoldFree
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

previewFree :: FreeF f a b -> Maybe (f b)
previewFree (Free fb) = Just fb
previewFree _ = Nothing

previewPure :: FreeF f a b -> Maybe a
previewPure (Pure a) = Just a
previewPure _ = Nothing


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

-- -- | Merge a deeply nested Event into a single Event
switchFree :: (Reflex t, MonadHold t m, MonadReader (NodeBuilderEnv t) m)
           => Free (Event t) a -> m (Event t a)
switchFree f = do
    let hoist (Identity x) = return x
    switchFreeT' hoist hoist f >>= \case
      Pure a -> postpone a
      Free e -> return e

-- switchFreeT :: NodeBuilder t m => FreeT (Event t) (PushM t) a -> m (Event t a)
-- switchFreeT f = do
--     e <- view postBuildEvent
--     let e' = flip pushAlways e $ \_ -> switchFreeT' id id f >>= \case
--           Pure a -> return $ a <$ e
--           Free e -> return e
--     switchPromptly never e'
--
--

-- Attributes
evt :: ( NodeBuilder t m
       , IsSettable w (Sequenceable m) a (attr w (Sequenceable m) b a) )
    => attr w (Sequenceable m) b a -> WOAttrib' w m (Event t a)
evt attr = WOAttrib $ \w e -> seqForEvent_ e $ setter attr w

-- | Transforms a IsSettable attribute to a WOAttribute. This uses lazy read on the incoming Dynamic
dyn :: ( NodeBuilder t m
       , IsSettable w (Sequenceable m) a (attr w (Sequenceable m) b a) )
    => attr w (Sequenceable m) b a -> WOAttrib' w m (Dynamic t a)
dyn attr = WOAttrib $ \w -> setter evtattr w <=< postponeCurrent
  where evtattr = evt attr


uDyn :: ( NodeBuilder t m
        , IsSettable w (Sequenceable m) a (attr w (Sequenceable m) b a)
        , Eq a )
     => attr w (Sequenceable m) b a -> WOAttrib' w m (UniqDynamic t a)
uDyn = (fromUniqDynamic >$<) . dyn

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
dyn' :: forall attr w t m b a.
        ( NodeBuilder t m
        , IsSettable w (Sequenceable m) a (attr w (Sequenceable m) b a)
        , IsSettable w m a (attr w m b a) )
     => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
     -> WOAttrib' w m (Dynamic t a)
dyn' attr = WOAttrib $ \w d -> do
              setter attr w =<< sample (current d)
              setter (evt (attr :: attr w (Sequenceable m) b a)) w (updated d)

uDyn' :: ( NodeBuilder t m
         , IsSettable w (Sequenceable m) a (attr w (Sequenceable m) b a)
         , IsSettable w m a (attr w m b a)
         , Eq a )
      => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
      -> WOAttrib' w m (UniqDynamic t a)
uDyn' attr = fromUniqDynamic >$< dyn' attr

-- implement NodeBuilder instance so that we don't need to keep lifting...
-- instance (MonadFix m, MonadHold t m, MonadSequenceEvent t m)
--         => MonadSequenceEvent t (AccStateT t f s im) where
--     Sequenceable (MonadSequenceEvent t m) = AccStateT t f s (Sequenceable m)
--     seqEventMaybe em = mdo
--         d <- watch
--         built <- lift . seqEvent $ flip pushAlways em $ \m -> do
--                       t <- sample behT
--                       return $ _runAccStateT m d [t]
--         let et = (mergeWith composeMaybe . snd) <$> built
--         behT :: Behavior t (Event t (s -> Maybe s)) <- hold (never :: Event t (s -> Maybe s)) et
--         adjustMaybe $ switch behT
--         return $ fmapMaybe fst built

instance MonadSequenceEvent t m => MonadSequenceEvent t (AccStateT t f s m) where
    type Sequenceable (AccStateT t f s m) = Sequenceable m
    seqEventMaybe = lift . seqEventMaybe
    seqEvent_ = lift . seqEvent_

instance MonadSequenceHold t m
  => MonadSequenceHold t (AccStateT t f s m) where
    type Finalizable (AccStateT t f s m) = Finalizable m
    seqHold ma emb = do
        d <- watch
        ((a, tsZ), erb) <- lift . seqHold (_runAccStateT ma d []) $
          ffor emb $ \mb -> _runAccStateT mb d []
        let et = mergeWith composeMaybe . snd <$> erb
            tz = mergeWith composeMaybe tsZ
        switchPromptly tz et >>= adjustMaybe
        return (a, fst <$> erb)
    addFinalizer = lift . addFinalizer

instance NodeBuilder t m => NodeBuilder t (AccStateT t f s m) where
