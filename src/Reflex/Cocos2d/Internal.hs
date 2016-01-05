{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Internal (
    mainScene
) where

import Data.Dependent.Sum (DSum (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Exception
import Control.Lens
import Reflex
import Reflex.Host.Class
import Reflex.Spider
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Scene
import Reflex.Cocos2d.Class

-- mostly borrowed from Reflex.Dom.Internal
data GraphEnv t = GraphEnv { _graphParent :: !Node
                           , _graphRunWithActions :: !(ActionTrigger t)
                           }

makeLenses ''GraphEnv

data GraphState t = GraphState { _graphPostBuild :: !(HostFrame t ())
                               , _graphVoidActions :: ![Event t (HostFrame t ())]
                               }

makeLenses ''GraphState

newtype Graph t a = Graph { unGraph :: ReaderT (GraphEnv t) (StateT (GraphState t) (HostFrame t)) a }

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

instance ( MonadIO (HostFrame t), MonadAsyncException (HostFrame t), Functor (HostFrame t)
         , ReflexHost t ) => NodeGraph t (Graph t) where
    askParent = Graph $ view graphParent
    schedulePostBuild a = Graph $ graphPostBuild %= (a>>)
    subGraph n child = Graph $ local (graphParent .~ n) $ unGraph child
    performEvent_ a = Graph $ graphVoidActions %= (a:)
    askRunWithActions = Graph $ view graphRunWithActions 

-- construct a new scene with a NodeGraph
mainScene :: Graph Spider () -> IO ()
mainScene graph = do
    scene <- createScene
    runSpiderHost $ runHostFrame $ mdo
        let runWithActions dm = runSpiderHost $ do
                va <- fireEventsAndRead dm $ sequence =<< readEvent voidActionHandle
                runHostFrame $ sequence_ va
        GraphState postBuild vas <- execStateT (runReaderT (unGraph graph) (GraphEnv (toNode scene) runWithActions)) (GraphState (return ()) [])
        postBuild
        voidActionHandle <- subscribeEvent $ mergeWith (>>) vas
        liftIO $ runScene scene
