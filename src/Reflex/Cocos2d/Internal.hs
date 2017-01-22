{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Internal
    (
      mainScene

    , NodeBuilder
    )
  where

import Data.Dependent.Sum ((==>))
import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Monad.Random
import Control.Lens
import Reflex
import Reflex.Host.Class

import Foreign.Ptr (castPtr)
import Foreign.Hoppy.Runtime (Decodable(..), CppPtr(..))

import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Scene
import Graphics.UI.Cocos2d.Director

import Reflex.Cocos2d.Class


-- mostly borrowed from Reflex.Dom.Internal
data NodeBuilderState t m = NodeBuilderState
    { _builderVoidActions :: ![Event t (m ())]
    , _builderFinalizers  :: m ()
    }

hoistBuilderState :: Reflex t => (m () -> n ()) -> NodeBuilderState t m -> NodeBuilderState t n
hoistBuilderState f (NodeBuilderState vas fin) = NodeBuilderState (fmap f <$> vas) (f fin)

instance Monad m => Monoid (NodeBuilderState t m) where
    mempty = NodeBuilderState [] (return ())
    (NodeBuilderState va0 fin0) `mappend` (NodeBuilderState va1 fin1) =
      -- the later finalizers should come first
      NodeBuilderState (va1 `mappend` va0) (fin1 >> fin0)

builderVoidActions ::
  forall t m.
  Lens' (NodeBuilderState t m) [Event t (m ())]
builderVoidActions f (NodeBuilderState act fin)
  = fmap (\ act' -> NodeBuilderState act' fin) (f act)
{-# INLINE builderVoidActions #-}
builderFinalizers ::
  forall t m.
  Lens' (NodeBuilderState t m) (m ())
builderFinalizers f (NodeBuilderState act fin)
  = fmap (\ fin' -> NodeBuilderState act fin') (f fin)
{-# INLINE builderFinalizers #-}

newtype NodeBuilder t m a = NodeBuilder (ReaderT (NodeBuilderEnv t) (StateT (NodeBuilderState t m) m) a)
    deriving ( Monad, Functor, Applicative
             , MonadReader (NodeBuilderEnv t)
             , MonadState (NodeBuilderState t m)
             , MonadFix, MonadIO
             , MonadException, MonadAsyncException
             , MonadSample t, MonadHold t
             , MonadReflexCreateTrigger t, MonadSubscribeEvent t
             , MonadRandom )

-- TODO: deriving instances for common monad transformers e.g., State,
-- Reader, RandT

instance MonadTrans (NodeBuilder t) where
    lift = NodeBuilder . lift . lift

instance MonadRef m => MonadRef (NodeBuilder t m) where
    type Ref (NodeBuilder t m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

-- run builder with a given env and empty state
runNodeBuilder :: NodeBuilder t m a -> NodeBuilderEnv t -> NodeBuilderState t m -> m (a, NodeBuilderState t m)
runNodeBuilder (NodeBuilder builder) env st =
    runStateT (runReaderT builder env) st

instance (Reflex t, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m, MonadIO m)
  => MonadSequenceEvent t (NodeBuilder t m) where
    type Sequenceable (NodeBuilder t m) = m
    seqEvent_ e = NodeBuilder $ builderVoidActions %= (e:)
    seqEventMaybe e = do
        run <- view runWithActions
        (eResult, trigger) <- newEventWithTriggerRef
        seqForEvent_ e $ \o -> do
            o >>= \case
              Just x -> liftIO $ readRef trigger >>= mapM_ (\t -> run ([t ==> x], return ()))
              _ -> return ()
        return eResult

instance BuilderMFunctor t (NodeBuilder t) where
    hoist f ba = do
        env <- ask
        -- XXX: hack, starting with empty state
        (a, st) <- lift $ f (runNodeBuilder ba env mempty)
        modify' (`mappend` (hoistBuilderState f st))
        return a

-- for things like RandT, we can achieve that with seqEvent coupled
-- with hoist, i.e.,
-- Event t (RandT g m' (Maybe a)) -> tf (RandT g m') (Event t a)
-- tf (RandT g m') (Event t a) -> tf m' (Event t a)
-- tf (tf m') (Maybe a) -> tf (tf m') (Event t a)

instance BuilderMMonad t (NodeBuilder t) where
    embed (f :: forall a. m a -> NodeBuilder t n a) ba = do
        env <- ask
        -- XXX: hack, starting with empty state
        (a, st) <- f (runNodeBuilder ba env mempty)
        -- for each new event, needs to change it to an actual firing event
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        let newSt = hoistBuilderState f st
            run = env ^. runWithActions
            onNewChildBuilt (va, fin) (NodeBuilderState vas' fin') =
              (let !e = mergeWith (>>) $! (va:reverse vas') in e, fin' >> fin)
            newVas' = ffor (newSt^.builderVoidActions) . fmap $ \bd -> do
              (postBuildE, postBuildTr) <- newEventWithTriggerRef
              let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
              (_, builderState) <- runNodeBuilder bd (env & postBuildEvent .~ postBuildE) mempty
              liftIO $ readRef newChildBuiltTriggerRef
                        >>= mapM_ (\t -> run ([t ==> builderState], firePostBuild))
        stateBeh <- accum onNewChildBuilt (never :: Event t (n ()), return ()) newChildBuilt
        let newChildVa = switch $ fst <$> stateBeh
        builderVoidActions %= ((newChildVa:) . (newVas'++))
        -- attach the finalizers
        addFinalizer $ do
          -- get the current combined finalizer and run it
          (_, currentFin) <- sample stateBeh
          liftIO $ putStrLn "[debug]: running combined finalizer from new child built"
          currentFin
          -- XXX: throw away the builder state: we assume the finalizers should be simple things
          void $ runNodeBuilder (newSt^.builderFinalizers) env mempty
        return a

instance ( Reflex t, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m
         , MonadIO m, MonadHold t m )
        => MonadSequenceHold t (NodeBuilder t m) where
    type Finalizable (NodeBuilder t m) = m
    seqHold init e = do
        p <- asks $ view parent
        oldState <- NodeBuilder $ get <* put (NodeBuilderState [] (return ()))
        result0 <- init
        state <- NodeBuilder $ get <* put oldState
        let voidAction0 = mergeWith (flip (>>)) (state^.builderVoidActions)
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        seqEvent_ <=< switchPromptly voidAction0 $
              mergeWith (flip (>>)) . view (_2.builderVoidActions) <$> newChildBuilt
        finalizerBeh <- hold (state^.builderFinalizers) (view (_2.builderFinalizers) <$> newChildBuilt)
        builderEnv <- ask
        let run = builderEnv ^. runWithActions
        seqForEvent_ e $ \bd -> do
            join $ sample finalizerBeh
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
            (r, builderState) <- flip (runNodeBuilder bd) mempty $
                builderEnv & parent .~ p
                           & postBuildEvent .~ postBuildE
            liftIO $ readRef newChildBuiltTriggerRef
                    >>= mapM_ (\t -> run ([t ==> (r, builderState)], firePostBuild))
        return (result0, fst <$> newChildBuilt)
    addFinalizer a = NodeBuilder $ builderFinalizers %= (a >>)

-- | Construct a new scene with a NodeBuilder
mainScene :: NodeBuilder Spider (HostFrame Spider) a -> IO a
mainScene bd = do
    scene <- scene_create
    dtor <- director_getInstance
    winSize <- decode =<< director_getWinSize dtor
    recRef <- newIORef (False, [], []) -- (running, saved_dm)
    result <- runSpiderHost $ mdo
        let processTrigger [] [] = writeIORef recRef (False, [], [])
            processTrigger [] aft = do
              writeIORef recRef (True, [], [])
              foldl (flip (>>)) (return ()) aft
              (_, saved, savedAft) <- readIORef recRef
              processTrigger saved savedAft
            processTrigger es aft = do
              writeIORef recRef (True, [], [])
              runSpiderHost $ do
                  va <- fireEventsAndRead es $ sequence =<< readEvent voidActionHandle
                  runHostFrame $ sequence_ va
              (_, saved, savedAft) <- readIORef recRef
              processTrigger saved (aft++savedAft)
            runWithActions (dm, aft) = do
              (running, saved, savedAft) <- readIORef recRef
              if running
                then writeIORef recRef (running, dm++saved, aft:savedAft)
                else processTrigger dm [aft]
        (postBuildE, postBuildTr) <- newEventWithTriggerRef
        -- tick events
        ticks <- newEventWithTrigger $ \tr -> liftIO $ do
            sch <- director_getScheduler dtor
            let target = castPtr $ toPtr dtor
            scheduler_scheduleWithInterval sch
              (\ss -> runWithActions ([tr ==> ss], return ()))
              target 0 False "ticks"
            return $ scheduler_unschedule sch "ticks" target
        (result, builderState) <- runHostFrame . flip (runNodeBuilder bd) mempty $
            NodeBuilderEnv (toNode scene) winSize postBuildE ticks runWithActions
        voidActionHandle <- subscribeEvent . mergeWith (flip (>>)) $ builderState^.builderVoidActions
        liftIO $ readRef postBuildTr >>= mapM_ (\t -> runWithActions ([t ==> ()], return ()))
        return result
    director_getInstance >>= flip director_runWithScene scene
    return result

