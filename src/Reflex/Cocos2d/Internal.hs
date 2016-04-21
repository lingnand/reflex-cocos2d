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
import Data.IORef
import Data.TLS.GHC
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Concurrent
import Control.Lens
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Scene
import Reflex.Cocos2d.Class

-- mostly borrowed from Reflex.Dom.Internal
data GraphEnv t = GraphEnv { _graphParent :: !Node
                           , _graphRunWithActions :: !([DSum (EventTrigger t) Identity] -> IO ())
                           }

makeLenses ''GraphEnv

data GraphState t = GraphState { _graphPostBuild :: !(HostFrame t ())
                               , _graphVoidActions :: ![Event t (HostFrame t ())]
                               }

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

instance ( MonadIO (HostFrame t), MonadAsyncException (HostFrame t), Functor (HostFrame t)
         , MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
         , ReflexHost t ) => NodeGraph t (Graph t) where
    askParent = Graph $ view graphParent
    schedulePostBuild a = Graph $ graphPostBuild %= (a>>)
    subGraph n (Graph c) = Graph $ local (graphParent .~ toNode n) c
    holdGraph p child0 newChild = do
        let p' = toNode p
        vas <- Graph $ use graphVoidActions <* (graphVoidActions .= [])
        result0 <- subGraph p' child0
        vas' <- Graph $ use graphVoidActions <* (graphVoidActions .= vas)
        let voidAction0 = mergeWith (>>) vas'
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        voidAction <- hold voidAction0 $ fmap snd newChildBuilt
        performEvent_ $ switch voidAction
        runWithActions <- askRunWithActions
        forH_ newChild $ \(Graph g) -> do
            removeAllChildren p'
            (r, GraphState postBuild vas) <- runStateT (runReaderT g (GraphEnv p' runWithActions)) (GraphState (return ()) [])
            liftIO $ readRef newChildBuiltTriggerRef
                     >>= mapM_ (\t -> runWithActions [t :=> Identity (r, mergeWith (>>) vas)])
            postBuild
        return (result0, fmap fst newChildBuilt)
    performEvent_ a = Graph $ graphVoidActions %= (a:)
    askRunWithActions = Graph $ view graphRunWithActions

-- the current problem with voidActions is that they only record
-- inscrutible IOs that do little for proper sequencing - if there is
-- nested calls to runWithActions then we have to forkIO to get over the
-- deadlocks
--
-- A better idea might be to send over data structures of the form
-- data LazyIO a = LazyIO { immediate :: IO a, later :: Maybe (LazyIO a) }
-- basically, we could save the blocks which could lead to deadlocks into
-- the `later` field, and when evaluating this LazyIO structure we first
-- complete all the `immediate` blocks before diving into the `later`
-- fields (which itself might have immediate and later). This basically
-- allows complex sequencing effects that should help getting over the
-- deadlocks

-- construct a new scene with a NodeGraph
mainScene :: Graph Spider a -> IO ()
mainScene (Graph g) = do
    scene <- createScene
    lock <- newMVar ()
    tlsRefs <- mkTLS $ newIORef (False, []) -- (running, saved_dm)
    runSpiderHost $ runHostFrame $ mdo
        let runWithActions [] = return ()
            runWithActions dm = do
              -- we use thread-local storage to store recursive invocations of
              tlsRef <- getTLS tlsRefs
              (running, saved) <- readIORef tlsRef
              if running
                then writeIORef tlsRef (running, saved++dm)
                else do
                  let process [] = writeIORef tlsRef (False, [])
                      process es = do
                        writeIORef tlsRef (True, [])
                        withMVar lock $ const . runSpiderHost $ do
                            va <- fireEventsAndRead es $ sequence =<< readEvent voidActionHandle
                            runHostFrame $ sequence_ va
                        (_, saved) <- readIORef tlsRef
                        process saved
                  process dm
        GraphState postBuild vas <- execStateT (runReaderT g (GraphEnv (toNode scene) runWithActions)) (GraphState (return ()) [])
        postBuild
        voidActionHandle <- subscribeEvent $ mergeWith (>>) vas
        runScene scene
    freeTLS tlsRefs -- Will we reach here? Probably not!
