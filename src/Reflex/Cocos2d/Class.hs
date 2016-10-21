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
    , NodeGraphEnv(NodeGraphEnv)
    , parent
    , windowSize
    , postBuildEvent
    , frameTicks
    , runWithActions
    , NodeGraph(..)
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
    , waitDynMaybe
    , waitDynMaybe'
    -- * Time
    , modulate
    -- * Rand
    , runRandEvent
    )
  where

import Data.Tuple (swap)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
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
import Reflex.State
import Reflex.Host.Class
import Graphics.UI.Cocos2d.Node

type Time = Float -- ^ in seconds

data NodeGraphEnv t = NodeGraphEnv
    { _parent  :: !Node
    , _windowSize     :: V2 Float
    , _postBuildEvent :: !(Event t ())
    , _frameTicks     :: !(Event t Time) -- ^ Ticks for each frame
    , _runWithActions :: !(([DSum (EventTrigger t) Identity], IO ()) -> IO ())
    }

frameTicks ::
  forall t. Lens' (NodeGraphEnv t) (Event t Time)
frameTicks
  f
  (NodeGraphEnv p winSize pb ticks run)
  = fmap
      (\ ticks'
         -> NodeGraphEnv p winSize pb ticks' run)
      (f ticks)
{-# INLINE frameTicks #-}
parent :: forall t. Lens' (NodeGraphEnv t) Node
parent
  f
  (NodeGraphEnv p winSize pb ticks run)
  = fmap
      (\ p'
         -> NodeGraphEnv p' winSize pb ticks run)
      (f p)
{-# INLINE parent #-}
postBuildEvent ::
  forall t. Lens' (NodeGraphEnv t) (Event t ())
postBuildEvent
  f
  (NodeGraphEnv p winSize pb ticks run)
  = fmap
      (\ pb'
         -> NodeGraphEnv p winSize pb' ticks run)
      (f pb)
{-# INLINE postBuildEvent #-}
runWithActions ::
  forall t.
  Lens' (NodeGraphEnv t) (([DSum (EventTrigger t) Identity],
                                 IO ())
                                -> IO ())
runWithActions
  f
  (NodeGraphEnv p winSize pb ticks run)
  = fmap
      (\ run'
         -> NodeGraphEnv p winSize pb ticks run')
      (f run)
{-# INLINE runWithActions #-}
windowSize ::
  forall t. Lens' (NodeGraphEnv t) (V2 Float)
windowSize
  f
  (NodeGraphEnv p winSize pb ticks run)
  = fmap
      (\ winSize'
         -> NodeGraphEnv p winSize' pb ticks run)
      (f winSize)
{-# INLINE windowSize #-}

class ( ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      , MonadReader (NodeGraphEnv t) m
      , MonadException m, MonadAsyncException m
      , MonadException (HostFrame t), MonadAsyncException (HostFrame t) )
      => NodeGraph t m where
    -- this one should be thread-safe
    -- askRunWithActionsAsync :: m (([DSum (EventTrigger t) Identity], IO ()) -> IO ())

    -- Composition primitives
    -- | Run a graph under a given node
    subGraph :: NodePtr n => n -> m a -> m a
    -- | Run a graph with the initial content and the updated content
    -- whenever the event updates
    holdGraph :: NodePtr n => n -> m a -> Event t (m b) -> m (a, Event t b)
    -- | sequencing
    buildEvent :: Event t (m a) -> m (Event t a)
    buildEvent_ :: Event t (m a) -> m ()

    -- misc operations
    -- lower in power (no construction power, but performance-wise better)
    -- this is mostly used inside the library to improve performance
    runEventMaybe :: Event t (HostFrame t (Maybe a)) -> m (Event t a)
    runEvent_ :: Event t (HostFrame t ()) -> m ()
    runEvent :: Event t (HostFrame t a) -> m (Event t a)

    filterMEvent :: (a -> HostFrame t Bool) -> Event t a -> m (Event t a)
    filterMEvent f e = runEventMaybe . ffor e $ \v -> do
                            b <- f v
                            return $ guard b >> return v
    onEventMaybe :: Event t a -> (a -> HostFrame t (Maybe b)) -> m (Event t b)
    onEventMaybe e = runEventMaybe . ffor e
    onEvent :: Event t a -> (a -> HostFrame t b) -> m (Event t b)
    onEvent e = runEvent . ffor e
    onEvent_ :: Event t a -> (a -> HostFrame t ()) -> m ()
    onEvent_ e = runEvent_ . ffor e

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 -<
(-<) :: (NodeGraph t m, NodePtr n) => m n -> m a -> m (n, a)
(-<) node child = do
    n <- node
    a <- subGraph n child
    return (n, a)

-- | Hold
-- e.g., @newChild & nodeBuilder -| child0@
infixr 2 -|
(-|) :: (NodeGraph t m, NodePtr n) => m n -> m a -> Event t (m a) -> m (n, Dynamic t a)
(-|) node child0 newChild = do
    n <- node
    (result0, newResult) <- holdGraph n child0 newChild
    dyn <- holdDyn result0 newResult
    return (n, dyn)

-- | View
-- e.g., @nodeBuilder -< child@
infixr 2 -<<
(-<<) :: (NodeGraph t m, NodePtr n) => m n -> Dynamic t (m a) -> m (n, Event t a)
(-<<) node child = do
    n <- node
    (_, evt) <- holdGraph n (return ()) =<< postponeCurrent child
    return (n, evt)

-- | Greedy Free
infixr 2 <-<
(<-<) :: (NodeGraph t m, NodePtr n) => m n -> FreeT (Event t) m a -> m (n, Event t a)
(<-<) node ft = mdo
    n <- node
    pe <- view postBuildEvent
    let startE = case result0 of
          Pure a -> return a <$ pe
          Free e -> e
    newFs <- switchPromptly startE $ fmapMaybe previewFree newResult
    (result0, newResult) <- holdGraph n (runFreeT ft) (runFreeT <$> newFs)
    return (n, fmapMaybe previewPure newResult)

-- | Convert to an Event that carries the first value in postBuild
postponeCurrent :: NodeGraph t m => Dynamic t a -> m (Event t a)
postponeCurrent d = do
    pe <- view postBuildEvent
    return $ leftmost [ pushAlways (const $ sample (current d)) pe
                      , updated d ]

-- lazy version of building dyn
buildDyn :: NodeGraph t m => Dynamic t (m a) -> m (Event t a)
buildDyn = buildEvent <=< postponeCurrent

-- strict version
buildDyn' :: NodeGraph t m => Dynamic t (m a) -> m (Dynamic t a)
buildDyn' d = do
    a <- join $ sample (current d)
    evt <- buildEvent (updated d)
    holdDyn a evt

runDyn :: NodeGraph t m => Dynamic t (HostFrame t a) -> m (Event t a)
runDyn = runEvent <=< postponeCurrent

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
switchFree :: NodeGraph t m => Free (Event t) a -> m (Event t a)
switchFree f = do
    let hoist (Identity x) = return x
    switchFreeT' hoist hoist f >>= \case
      Pure a -> fmap (a <$) $ view postBuildEvent
      Free e -> return e

switchFreeT :: NodeGraph t m => FreeT (Event t) (PushM t) a -> m (Event t a)
switchFreeT f = do
    e <- view postBuildEvent
    let e' = flip pushAlways e $ \_ -> switchFreeT' id id f >>= \case
          Pure a -> return $ a <$ e
          Free e -> return e
    switchPromptly never e'

-- | Depth visit the nested events in the FreeT
runGreedyFreeT :: NodeGraph t m => FreeT (Event t) (HostFrame t) a -> m (Event t a)
runGreedyFreeT ft = do
    pe <- view postBuildEvent
    rec newFs <- switchPromptly (ft <$ pe) $ fmapMaybe previewFree e'
        e' <- runEvent $ runFreeT <$> newFs
    return $ fmapMaybe previewPure e'

buildGreedyFreeT :: NodeGraph t m => FreeT (Event t) m a -> m (Event t a)
buildGreedyFreeT ft = runFreeT ft >>= \case
    Pure v -> fmap (v <$) $ view postBuildEvent
    Free startE -> mdo
      newFs <- switchPromptly startE $ fmapMaybe previewFree e'
      e' <- buildEvent $ runFreeT <$> newFs
      return $ fmapMaybe previewPure e'

-- | Wait for the first occurrence
waitEvent :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m a
waitEvent = liftF

-- | Wait for the first occurrence and include the future occurrences in return
waitEvent' :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m (a, Event t a)
waitEvent' e = (,e) <$> liftF e

-- | Wait for the Dynamic to turn from Nothing to Just
waitDynMaybe :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m a
waitDynMaybe dyn = lift (sample $ current dyn) >>= \case
    Just a -> return a
    _ -> waitEvent $ fmapMaybe id (updated dyn)

-- | Wait for the first Just value, and include the future values in return
waitDynMaybe' :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m (a, Event t a)
waitDynMaybe' dyn = (,fmapMaybe id $ updated dyn) <$> waitDynMaybe dyn


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

runRandEvent :: (MonadIO m, Reflex t, MonadHold t m, MonadFix m)
             => Event t (Rand StdGen a) -> m (Event t a)
runRandEvent rands = do
    g <- liftIO newStdGen
    mapAccum_ (\g comp -> swap $ runRand comp g) g rands


-- implement NodeGraph instance so that we don't need to keep lifting...
instance NodeGraph t m => NodeGraph t (AccStateT t f s m) where
    -- askRunWithActionsAsync = lift askRunWithActionsAsync
    subGraph n m = AccStateT . StateT $ \ts -> ReaderT $ \d -> subGraph n (_runAccStateT m d ts)
    holdGraph n ma emb = do
      d <- watch
      ((a, tsZ), erb) <- lift $ holdGraph n (_runAccStateT ma d []) $ ffor emb $ \mb -> _runAccStateT mb d []
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
    runEventMaybe = lift . runEventMaybe
    runEvent_ = lift . runEvent_
    runEvent = lift . runEvent
