{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Internal
    (
      mainScene
    ) where

import Data.Dependent.Sum (DSum (..))
import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Lens
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Scene
import Reflex.Cocos2d.Class

-- mostly borrowed from Reflex.Dom.Internal
data GraphEnv t = GraphEnv { _graphParent :: !Node
                           , _graphPostBuildEvent :: !(Event t ())
                           , _graphRunWithActions :: !([DSum (EventTrigger t) Identity] -> IO ())
                           }

makeLenses ''GraphEnv

data GraphState t = GraphState { _graphVoidActions :: ![Event t (HostFrame t ())] }

makeLenses ''GraphState

newtype Graph t a = Graph (ReaderT (GraphEnv t) (StateT (GraphState t) (HostFrame t)) a)

deriving instance Functor (HostFrame t) => Functor (Graph t)
deriving instance (Monad (HostFrame t), Applicative (HostFrame t)) => Applicative (Graph t)
deriving instance Monad (HostFrame t) => Monad (Graph t)
deriving instance MonadFix (HostFrame t) => MonadFix (Graph t)
deriving instance MonadIO (HostFrame t) => MonadIO (Graph t)
deriving instance MonadException (HostFrame t) => MonadException (Graph t)
deriving instance MonadAsyncException (HostFrame t) => MonadAsyncException (Graph t)

instance MonadSample t (HostFrame t) => MonadSample t (Graph t) where
    sample = Graph . lift . lift . sample

instance MonadHold t (HostFrame t) => MonadHold t (Graph t) where
    hold v0 = Graph . lift . lift . hold v0

instance MonadReflexCreateTrigger t (HostFrame t) => MonadReflexCreateTrigger t (Graph t) where
    newEventWithTrigger = Graph . lift . lift . newEventWithTrigger
    newFanEventWithTrigger = Graph . lift . lift . newFanEventWithTrigger

instance MonadSubscribeEvent t (HostFrame t) => MonadSubscribeEvent t (Graph t) where
    subscribeEvent = Graph . lift . lift . subscribeEvent

instance MonadRef (HostFrame t) => MonadRef (Graph t) where
    type Ref (Graph t) = Ref (HostFrame t)
    newRef = Graph . lift . lift . newRef
    readRef = Graph . lift . lift . readRef
    writeRef r = Graph . lift . lift . writeRef r

instance MonadIO (Graph t) => PrimMonad (Graph t) where
    type PrimState (Graph t) = PrimState IO
    primitive = liftIO . primitive

instance ( MonadIO (HostFrame t), MonadAsyncException (HostFrame t), Functor (HostFrame t)
         , MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
         , ReflexHost t ) => NodeGraph t (Graph t) where
    askParent = Graph $ view graphParent
    askPostBuildEvent = Graph $ view graphPostBuildEvent
    subG n (Graph c) = Graph $ local (graphParent .~ toNode n) c
    holdG p child0 newChild = do
        let p' = toNode p
        vas <- Graph $ use graphVoidActions <* (graphVoidActions .= [])
        result0 <- subG p' child0
        vas' <- Graph $ use graphVoidActions <* (graphVoidActions .= vas)
        let voidAction0 = mergeWith (flip (>>)) vas'
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        voidAction <- hold voidAction0 $ snd <$> newChildBuilt
        sequenceH_ $ switch voidAction
        runWithActions <- askRunWithActions
        sequenceH_ . ffor newChild $ \(Graph g) -> do
            removeAllChildren p'
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            (r, GraphState vas) <- runStateT (runReaderT g (GraphEnv p' postBuildE runWithActions)) (GraphState [])
            liftIO $ do
              readRef newChildBuiltTriggerRef
                >>= mapM_ (\t -> runWithActions [t :=> Identity (r, mergeWith (flip (>>)) vas)])
              readRef postBuildTr >>= mapM_ (\t -> putStrLn "GOT POST BUILD TRIGGER" >> runWithActions [t :=> Identity ()])
        return (result0, fst <$> newChildBuilt)
    sequenceG newChild = do
        p <- askParent
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        voidActionDyn <- foldDynMaybe ?? never ?? (snd <$> newChildBuilt) $ \vas accE -> do
                            _ <- listToMaybe vas
                            return $ mergeWith (flip (>>)) (accE:vas)
        sequenceH_ $ switchPromptlyDyn voidActionDyn
        runWithActions <- askRunWithActions
        sequenceH_ . ffor newChild $ \(Graph g) -> do
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            (r, GraphState vas) <- runStateT (runReaderT g (GraphEnv p postBuildE runWithActions)) (GraphState [])
            let vas' = ((liftIO $ putStrLn "Oh No") <$ postBuildE) : vas
            liftIO $ do
              newCT <- readRef newChildBuiltTriggerRef
              forM_ newCT $ \t -> runWithActions [t :=> Identity (r, vas')]
              postBuildT <- readRef postBuildTr
              putStrLn $ "GOT POST BUILD TRIGGER: " ++ (show $ () <$ postBuildT)
              forM_ postBuildT $ \t -> runWithActions [t :=> Identity ()]
        return $ fst <$> newChildBuilt
    sequenceH_ a = Graph $ graphVoidActions %= (a:)
    -- this works because it forces a nested runWithActions call which has
    -- to be pushed pending and only fired when the current runWithActions
    -- finishes
    delay = mapG return
    askRunWithActions = Graph $ view graphRunWithActions

-- the reason why we could do away with LOCKs and thread local storage in
-- `runWithActions`:
-- 1. we expect the source of any event, eventually, is inside a javascript
--    foreign functional callback
-- 2. that means `runWithActions` always runs inside a function callback
-- 3. when inside a function callback, the ghcjs runtime has NO way to
--    reschedule other things to run OR interrupt the execution of the
--    callback (given that the haskell callback is constructed as
--    synchronous)
-- 4. this means there can be no race condition introduced by the ghcjs
--    runtime
--
-- Conclusion:
-- 1. make sure that every event e.g., constructed with
--    newEventWithTrigger, sends the events in a callback
-- 2. OR the event depends on a event that is constructed in the way of 1

-- construct a new scene with a NodeGraph
mainScene :: Graph Spider a -> IO ()
mainScene (Graph g) = do
    scene <- createScene
    recRef <- newIORef (False, []) -- (running, saved_dm)
    runSpiderHost $ runHostFrame $ mdo
        (postBuildE, postBuildTr) <- newEventWithTriggerRef
        let runWithActions [] = return ()
            runWithActions dm = do
              (running, saved) <- readIORef recRef
              if running
                then writeIORef recRef (running, saved++dm)
                else do
                  let process [] = writeIORef recRef (False, [])
                      process es = do
                        writeIORef recRef (True, [])
                        runSpiderHost $ do
                            va <- fireEventsAndRead es $ sequence =<< readEvent voidActionHandle
                            runHostFrame $ sequence_ va
                        (_, saved) <- readIORef recRef
                        process saved
                  process dm
        GraphState vas <- execStateT (runReaderT g (GraphEnv (toNode scene) postBuildE runWithActions)) (GraphState [])
        liftIO $ readRef postBuildTr >>= mapM_ (\t -> runWithActions [t :=> Identity ()])
        voidActionHandle <- subscribeEvent $ mergeWith (flip (>>)) vas
        runScene scene
