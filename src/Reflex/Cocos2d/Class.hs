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

    , EventSequencer(..)
    , EventSequenceHolder(..)

    , NodeBuilder(..)

    , postponeCurrent
    , postpone
    )
  where

import Data.Monoid
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
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Exception
import Control.Monad.Random
import Control.Lens
import Reflex
import Reflex.Cocos2d.Attributes
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


class (Reflex t, Monad im, MonadHold t m) => EventSequenceHolder t im m where
    seqHold :: (im a) -> Event t (im b) -> m (a, Event t b)
    seqHoldDyn :: (im a) -> Event t (im a) -> m (Dynamic t a)
    seqHoldDyn init e = seqHold init e >>= uncurry holdDyn
    -- non-strict evaluation of dynamic
    seqDyn  :: Dynamic t (im a) -> m (Event t a)
    -- strict evaluation
    seqDyn' :: Dynamic t (im a) -> m (Dynamic t a)
    seqDyn' d = sample (current d) >>= flip seqHoldDyn (updated d)


class (Reflex t, Monad im, Monad m) => EventSequencer t im m where
    seqEventMaybe :: Event t (im (Maybe a)) -> m (Event t a)
    seqEventMaybe = fmap (fmapMaybe id) . seqEvent
    seqEvent :: Event t (im a) -> m (Event t a)
    seqEvent = seqEventMaybe . fmap (Just <$>)
    seqEvent_ :: Event t (im ()) -> m ()
    seqEvent_ = void . seqEvent

    seqMapEventMaybe :: (a -> im (Maybe b)) -> Event t a -> m (Event t b)
    seqMapEventMaybe f = seqEventMaybe . fmap f
    seqMapEvent :: (a -> im b) -> Event t a -> m (Event t b)
    seqMapEvent f = seqEvent . fmap f
    seqMapEvent_ ::  (a -> im ()) -> Event t a -> m ()
    seqMapEvent_ f = seqEvent_ . fmap f

    forEventMaybe :: Event t a -> (a -> im (Maybe b)) -> m (Event t b)
    forEventMaybe e = seqEventMaybe . ffor e
    forEvent :: Event t a -> (a -> im b) -> m (Event t b)
    forEvent e = seqEvent . ffor e
    forEvent_ :: Event t a -> (a -> im ()) -> m ()
    forEvent_ e = seqEvent_ . ffor e

class Reflex t => SequenceAccumulator t f | f -> t where
    seqAccum :: (MonadHold t m, MonadFix m, EventSequencer t im m)
             => (a -> b -> im a) -> a -> Event t b -> m (f a)
    seqAccum f = seqAccumMaybe $ \v o -> Just <$> f v o

    seqAccumMaybe :: (MonadHold t m, MonadFix m, EventSequencer t im m)
                  => (a -> b -> im (Maybe a)) -> a -> Event t b -> m (f a)

    seqMapAccum :: (MonadHold t m, MonadFix m, EventSequencer t im m)
                => (a -> b -> im (a, c)) -> a -> Event t b -> m (f a, Event t c)
    seqMapAccum f = seqMapAccumMaybe $ \v o -> bimap Just Just <$> f v o

    seqMapAccumMaybe :: (MonadHold t m, MonadFix m, EventSequencer t im m)
                     => (a -> b -> im (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)

seqMapAccum_ :: forall t im m a b c. (Reflex t, MonadHold t m, MonadFix m, EventSequencer t im m)
             => (a -> b -> im (a, c)) -> a -> Event t b -> m (Event t c)
seqMapAccum_ f z e = do
    (_ :: Dynamic t a, result) <- seqMapAccum f z e
    return result

seqMapAccumMaybe_ :: forall t im m a b c.
  (Reflex t, MonadHold t m, MonadFix m, EventSequencer t im m)
  => (a -> b -> im (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
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

instance {-# INCOHERENT #-}
    (MonadIO m, Reflex t, MonadHold t m, MonadFix m, EventSequencer t im m)
    => EventSequencer t (RandT StdGen im) m where
    seqEventMaybe rands = do
      g <- liftIO newStdGen
      seqMapAccumMaybe_ (\g ma -> first Just . swap <$> runRandT ma g) g rands

class
  ( Reflex t
  , MonadReflexCreateTrigger t host, MonadSubscribeEvent t host
  , MonadSample t host, MonadHold t host, MonadFix host
  , MonadRef host, Ref host ~ Ref IO
  , MonadIO host
  , MonadReader (NodeBuilderEnv t) m
  , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
  , MonadSample t m, MonadHold t m, MonadFix m
  , MonadRef m, Ref m ~ Ref IO
  , MonadIO m
  , EventSequencer t host m
  , EventSequencer t m m
  , EventSequenceHolder t m m
  ) => NodeBuilder t host m | m -> host where
    -- add finalizer into the builder
    addFinalizer :: host () -> m ()

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
seqHoldFree :: forall t im m a.
               ( MonadFix m, MonadHold t m
               , MonadReader (NodeBuilderEnv t) m
               , EventSequenceHolder t im m )
            => FreeT (Event t) im a -> m (Event t a)
seqHoldFree ft = mdo
    let startE = case result0 of
          Pure a -> never :: Event t (FreeT (Event t) im a)
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

-- seqFree :: (MonadFix m, EventSequencer t im (NodeBuilder t m))
--         => FreeT (Event t) im a -> NodeBuilder t m (Event t a)
-- seqFree ft = runFreeT ft >>= \case
--     Pure v -> postpone v
--     Free startE -> mdo
--       newFs <- switchPromptly startE $ fmapMaybe previewFree e'
--       e' <- seqEvents $ runFreeT <$> newFs
--       return $ fmapMaybe previewPure e'

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
-- | Transforms a IsSettable attribute to a WOAttribute. This uses lazy read on the incoming Dynamic
dyn :: (EventSequencer t im m, IsSettable w im a (attr w im b a), MonadReader (NodeBuilderEnv t) m)
    => attr w im b a -> WOAttrib' w m (Dynamic t a)
dyn attr = WOAttrib $ \w -> setter evtattr w <=< postponeCurrent
  where evtattr = evt attr


uDyn :: ( EventSequencer t im m, IsSettable w im a (attr w im b a), MonadReader (NodeBuilderEnv t) m
        , Eq a )
     => attr w im b a -> WOAttrib' w m (UniqDynamic t a)
uDyn = (fromUniqDynamic >$<) . dyn

evt :: (EventSequencer t im m, IsSettable w im a (attr w im b a))
    => attr w im b a -> WOAttrib' w m (Event t a)
evt attr = WOAttrib $ \w e -> forEvent_ e $ setter attr w


-- | Locally modulate the ticks in the environment
-- implement NodeBuilder instance so that we don't need to keep lifting...
-- instance EventSequencer t im m => EventSequencer t im (AccStateT t f s m) where
--     seqEventMaybe em = do
--         d <- watch
--         built <- lift . seqEvent $ flip pushAlways em $ \m -> do
--                       t <- sample behT
--                       return $ _runAccStateT m d [t]
--         let et = (mergeWith composeMaybe . snd) <$> built
--         behT :: Behavior t (Event t (s -> Maybe s)) <- hold (never :: Event t (s -> Maybe s)) et
--         adjustMaybe $ switch behT
--         return $ fmapMaybe fst built


    -- askRunWithActionsAsync = lift askRunWithActionsAsync
    -- subNode n m = AccStateT . StateT $ \ts -> ReaderT $ \d -> subNode n (_runAccStateT m d ts)
    -- holdNodes ma emb = do
    --   d <- watch
    --   ((a, tsZ), erb) <- lift $ holdNodes (_runAccStateT ma d []) $ ffor emb $ \mb -> _runAccStateT mb d []
    --   let et = (mergeWith composeMaybe . snd) <$> erb
    --       tz = mergeWith composeMaybe tsZ
    --   switchPromptly tz et >>= adjustMaybe
    --   return (a, fst <$> erb)
    -- buildEvent em = mdo
    --   d <- watch
    --   built <- lift . buildEvent $ flip pushAlways em $ \m -> do
    --                 t <- sample behT
    --                 return $ _runAccStateT m d [t]
    --   let et = (mergeWith composeMaybe . snd) <$> built
    --   behT :: Behavior t (Event t (s -> Maybe s)) <- hold (never :: Event t (s -> Maybe s)) et
    --   adjustMaybe $ switch behT
    --   return $ fst <$> built
    -- buildEvent_ = void . buildEvent
    -- seqEventMaybe = lift . seqEventMaybe
    -- seqEvent_ = lift . seqEvent_
    -- seqEvent = lift . seqEvent
    --
