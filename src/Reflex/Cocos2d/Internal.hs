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
    ( mainScene
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
import Control.Lens
import Reflex
import Reflex.Host.Class

import Foreign.Ptr (castPtr)
import Foreign.Hoppy.Runtime (Decodable(..), CppPtr(..))

import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Scene
import Graphics.UI.Cocos2d.Director

import Reflex.Cocos2d.Class

-- spec
-- class ( ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadHold t m
--       , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
--       , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
--       , MonadReader (NodeTreeEnv t) m
--       , EventSequencer t (HostFrame t) m
--       , EventSequencer t m m -- sequence itself
--       , MonadException m, MonadAsyncException m
--       , MonadException (HostFrame t), MonadAsyncException (HostFrame t)
--       )
--       => NodeBuilder t m where
--     -- this one should be thread-safe
--     -- askRunWithActionsAsync :: m (([DSum (EventTrigger t) Identity], IO ()) -> IO ())
--
--     -- | Run a tree with the initial content and the updated content
--     -- whenever the event updates
--     holdNodes :: NodePtr n => m a -> Event t (m b) -> m (a, Event t b)

-- mostly borrowed from Reflex.Dom.Internal
data BuilderState t m = BuilderState
    { _builderVoidActions :: ![Event t (m ())]
    , _builderFinalizers  :: m ()
    }


-- TODO: fix this
builderVoidActions ::
  forall t.
  Lens [Event t (m ())] [Event t (IO ())]
builderVoidActions f (BuilderState act fin)
  = fmap (\ act' -> BuilderState act' fin) (f act)
{-# INLINE builderVoidActions #-}

builderFinalizers ::
  forall t.
  Lens (IO ()) (IO ())
builderFinalizers f (BuilderState act fin)
  = fmap (\ fin' -> BuilderState act fin') (f fin)
{-# INLINE builderVoidActions #-}

newtype NodeBuilder t m a = NodeBuilder (ReaderT (NodeBuilderEnv t) (StateT (BuilderState t) m) a)

deriving instance Monad m => MonadReader (NodeBuilderEnv t) (NodeBuilder t m)
deriving instance Functor m => Functor (NodeBuilder t m)
deriving instance Monad m => Applicative (NodeBuilder t m)
deriving instance Monad m => Monad (NodeBuilder t m)
deriving instance MonadFix m => MonadFix (NodeBuilder t m)
deriving instance MonadIO m => MonadIO (NodeBuilder t m)
deriving instance MonadException m => MonadException (NodeBuilder t m)
deriving instance MonadAsyncException m => MonadAsyncException (NodeBuilder t m)

addFinalizer :: m () -> NodeBuilder t m ()
addFinalizer a = builderFinalizers %= (a:)

runBuilder
  :: NodeBuilder t m a -> NodeBuilderEnv t -> BuilderState t -> m (a, BuilderState t)
runBuilder (NodeBuilder builder) env = runStateT $ runReaderT builder env

instance (Reflex t, MonadSample t (HostFrame t)) => MonadSample t (NodeBuilder t) where
    sample = NodeBuilder . lift . lift . sample

instance (Reflex t, MonadHold t (HostFrame t)) => MonadHold t (NodeBuilder t) where
    hold v0 = NodeBuilder . lift . lift . hold v0
    holdDyn v0 = NodeBuilder . lift . lift . holdDyn v0
    holdIncremental v0 = NodeBuilder . lift . lift . holdIncremental v0

instance MonadReflexCreateTrigger t (HostFrame t) => MonadReflexCreateTrigger t (NodeBuilder t) where
    newEventWithTrigger = NodeBuilder . lift . lift . newEventWithTrigger
    newFanEventWithTrigger = NodeBuilder . lift . lift . newFanEventWithTrigger

instance (Reflex t, MonadSubscribeEvent t (HostFrame t)) => MonadSubscribeEvent t (NodeBuilder t) where
    subscribeEvent = NodeBuilder . lift . lift . subscribeEvent

instance MonadRef (HostFrame t) => MonadRef (NodeBuilder t) where
    type Ref (NodeBuilder t) = Ref (HostFrame t)
    newRef = NodeBuilder . lift . lift . newRef
    readRef = NodeBuilder . lift . lift . readRef
    writeRef r = NodeBuilder . lift . lift . writeRef r

instance EventSequencer t IO (NodeBuilder t) where
    seqEvent_ e = NodeBuilder $ builderVoidActions %= (e:)
    seqEventMaybe e = do
      run <- view runWithActions
      (eResult, trigger) <- newEventWithTriggerRef
      forEvent_ e $ \o -> do
          o >>= \case
            Just x -> readRef trigger >>= mapM_ (\t -> run ([t ==> x], return ()))
            _ -> return ()
      return eResult

instance EventSequencer t (NodeBuilder t) (NodeBuilder t) where
    seqEvent_ e = do
        vas <- NodeBuilder $ use builderVoidActions <* (builderVoidActions .= [])
        result0 <- subBuilder p' child0
        vas' <- NodeBuilder $ use builderVoidActions <* (builderVoidActions .= vas)
        let voidAction0 = mergeWith (flip (>>)) vas'
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        runEvent_ =<< switchPromptly voidAction0 (snd <$> newChildBuilt)
        builderEnv <- ask
        let run = builderEnv ^. runWithActions
        onEvent_ newChild $ \(NodeBuilder g) -> do
            liftIO $ node_removeAllChildren p'
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
            (r, BuilderState vas)
              <- runStateT
                  (runReaderT g $ builderEnv & parent .~ p'
                                           & postBuildEvent .~ postBuildE)
                  (BuilderState [])
            liftIO $ readRef newChildBuiltTriggerRef
                      >>= mapM_ (\t -> run ([t ==> (r, mergeWith (flip (>>)) vas)], firePostBuild))
        return (result0, fst <$> newChildBuilt)
    seqEventMaybe e = undefined

instance EventSequenceHolder t (NodeBuilder t) (NodeBuilder t) where
    pass...


instance ( MonadIO (HostFrame t), Functor (HostFrame t)
         , MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
         , MonadException (HostFrame t), MonadAsyncException (HostFrame t)
         , ReflexHost t ) => NodeBuilder t (NodeBuilder t) where
    subBuilder n = local (parent .~ toNode n)
    holdBuilder p child0 newChild = do
        let p' = toNode p
        vas <- NodeBuilder $ use builderVoidActions <* (builderVoidActions .= [])
        result0 <- subBuilder p' child0
        vas' <- NodeBuilder $ use builderVoidActions <* (builderVoidActions .= vas)
        let voidAction0 = mergeWith (flip (>>)) vas'
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        runEvent_ =<< switchPromptly voidAction0 (snd <$> newChildBuilt)
        builderEnv <- ask
        let run = builderEnv ^. runWithActions
        onEvent_ newChild $ \(NodeBuilder g) -> do
            liftIO $ node_removeAllChildren p'
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
            (r, BuilderState vas)
              <- runStateT
                  (runReaderT g $ builderEnv & parent .~ p'
                                           & postBuildEvent .~ postBuildE)
                  (BuilderState [])
            liftIO $ readRef newChildBuiltTriggerRef
                      >>= mapM_ (\t -> run ([t ==> (r, mergeWith (flip (>>)) vas)], firePostBuild))
        return (result0, fst <$> newChildBuilt)
    buildEvent = undefined
    -- buildEvent newChild = do
    --     (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
    --     let onNewChildBuilt :: Event t (HostFrame t ()) -> (a, [Event t (HostFrame t ())]) -> Maybe (Event t (HostFrame t ()))
    --         onNewChildBuilt _ (_, []) = Nothing
    --         onNewChildBuilt acc (_, vas) = Just $ mergeWith (>>) (acc:reverse vas)
    --     runEvent_ . switch =<< accumMaybe onNewChildBuilt (never :: Event t (HostFrame t ())) newChildBuilt
    --     builderEnv <- ask
    --     let run = builderEnv ^. runWithActions
    --     onEvent_ newChild $ \(NodeBuilder g) -> do
    --         (postBuildE, postBuildTr) <- newEventWithTriggerRef
    --         let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
    --         (r, BuilderState vas)
    --           <- runStateT
    --               (runReaderT g $ builderEnv & postBuildEvent .~ postBuildE)
    --               (BuilderState [])
    --         liftIO $ readRef newChildBuiltTriggerRef
    --                   >>= mapM_ (\t -> run ([t ==> (r, vas)], firePostBuild))
    --     return $ fst <$> newChildBuilt
    buildEvent_ = void . buildEvent
    runEvent_ a = NodeBuilder $ builderVoidActions %= (a:)
    runEventMaybe e = do
      run <- view runWithActions
      (eResult, trigger) <- newEventWithTriggerRef
      onEvent_ e $ \o -> do
          o >>= \case
            Just x -> liftIO $ readRef trigger >>= mapM_ (\t -> run ([t ==> x], return ()))
            _ -> return ()
      return eResult
    runEvent = runEventMaybe . fmap (Just <$>)
    -- | Generate a new Event that delays the input Event by some frame
    -- (normally fired in the immediate next frame); similar to
    -- setTimeout(0)
    -- this works because it forces a nested runWithActions call which has
    -- to be pushed pending and only fired when the current runWithActions
    -- finishes
    -- delay' = runEvent . fmap return
    -- askRunWithActionsAsync = NodeBuilder $ do
    --   runWithActions <- view builderRunWithActions
    --   sch <- liftIO $ director_getInstance >>= director_getScheduler
    --   return $ scheduler_performFunctionInCocosThread sch . runWithActions

-- | Construct a new scene with a NodeBuilder
mainScene :: NodeBuilder Spider a -> IO ()
mainScene (NodeBuilder g) = do
    scene <- scene_create
    dtor <- director_getInstance
    winSize <- decode =<< director_getWinSize dtor
    recRef <- newIORef (False, [], []) -- (running, saved_dm)
    runSpiderHost $ mdo
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
        BuilderState vas
          <- runHostFrame $
              execStateT
                (runReaderT g (NodeBuilderEnv (toNode scene) winSize postBuildE ticks runWithActions))
                (BuilderState [])
        voidActionHandle <- subscribeEvent $ mergeWith (flip (>>)) vas
        liftIO $ readRef postBuildTr >>= mapM_ (\t -> runWithActions ([t ==> ()], return ()))
    director_getInstance >>= flip director_runWithScene scene
