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
import Control.Monad
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

newtype ELoop t a = ELoop (ReaderT ([DSum (EventTrigger t)] -> IO ()) IO a) deriving (Monad, Applicative, Functor, MonadIO)

-- mostly borrowed from Reflex.Dom.Internal
data GraphEnv t = GraphEnv { _graphParent :: !Node
                           , _graphPostEventLoop :: !(ELoop t () -> IO ())
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
    type EventLoop (Graph t) = ELoop t
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
        postEl <- askPostEventLoop
        forH_ newChild $ \(Graph g) -> do
            removeAllChildren p'
            (r, GraphState postBuild vas) <- runStateT (runReaderT g (GraphEnv p' postEl)) (GraphState (return ()) [])
            liftIO . postEl $ mapM_ (\t -> runWithActions [t :=> (r, mergeWith (>>) vas)])
                              =<< readRef newChildBuiltTriggerRef
            postBuild
        return (result0, fmap fst newChildBuilt)
    performEvent_ a = Graph $ graphVoidActions %= (a:)

instance MonadRef (ELoop t) where
    type Ref (ELoop t) = Ref IO
    newRef = ELoop . lift . newRef
    readRef = ELoop . lift . readRef
    writeRef r = ELoop . lift . writeRef r

instance (MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => HasEventLoop t (ELoop t) (Graph t) where
    askPostEventLoop = Graph $ view graphPostEventLoop
    runWithActions dm = ELoop $ do
      r <- ask
      liftIO $ r dm

-- construct a new scene with a NodeGraph
mainScene :: Graph Spider a -> IO ()
mainScene (Graph g) = do
    scene <- createScene
    frames <- newChan
    runSpiderHost $ runHostFrame $ mdo
        let postEventLoop = writeChan frames
            runWithActions dm = runSpiderHost $ do
                va <- fireEventsAndRead dm $ sequence =<< readEvent voidActionHandle
                runHostFrame $ sequence_ va
            ELoop el = forever . join $ liftIO $ readChan frames
        GraphState postBuild vas <- execStateT (runReaderT g (GraphEnv (toNode scene) postEventLoop)) (GraphState (return ()) [])
        postBuild
        voidActionHandle <- subscribeEvent $ mergeWith (>>) vas
        runScene scene
        -- the async thread that runs all event propagation
        void . liftIO . forkIO $ runReaderT el runWithActions
