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
    scene,
    Cocos2dHost,
    runCocos2dHost
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

instance ( MonadIO (HostFrame t), MonadAsyncException (HostFrame t), Functor (HostFrame t)
         , ReflexHost t, MonadReflexAction t (Graph t) ) => NodeGraph t (Graph t) where
    askParent = Graph $ view graphParent
    schedulePostBuild a = Graph $ graphPostBuild %= (a>>)
    subGraph n child = Graph $ local (graphParent .~ n) $ unGraph child

instance ReflexHost t => MonadReflexAction t (Graph t) where
    addVoidAction a = Graph $ graphVoidActions %= (a:)
    askRunWithActions = Graph $ view graphRunWithActions 

data Cocos2dEnv t = Cocos2dEnv { _cocos2dRunWithActions :: !(ActionTrigger t) }

makeLenses ''Cocos2dEnv

data Cocos2dState t = Cocos2dState { _cocos2dVoidActions :: ![Event t (HostFrame t ())] }

makeLenses ''Cocos2dState

newtype Cocos2dHost t m a = Cocos2dHost { unCocos2dHost :: ReaderT (Cocos2dEnv t) (StateT (Cocos2dState t) m) a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance MonadTrans (Cocos2dHost t) where
    lift = Cocos2dHost . lift . lift

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Cocos2dHost t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger = lift . newFanEventWithTrigger

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (Cocos2dHost t m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (Cocos2dHost t m) where
  type ReadPhase (Cocos2dHost t m) = ReadPhase m
  fireEventsAndRead dm = lift . fireEventsAndRead dm
  runHostFrame = lift . runHostFrame

instance MonadReflexHost t m => MonadReflexAction t (Cocos2dHost t m) where
    addVoidAction a = Cocos2dHost $ cocos2dVoidActions %= (a:)
    askRunWithActions = Cocos2dHost $ view cocos2dRunWithActions

instance MonadSample t m => MonadSample t (Cocos2dHost t m) where
  {-# INLINE sample #-}
  sample = Cocos2dHost . lift . lift . sample

instance MonadHold t m => MonadHold t (Cocos2dHost t m) where
  {-# INLINE hold #-}
  hold v0 = Cocos2dHost . lift . lift . hold v0

runCocos2dHost :: Cocos2dHost Spider SpiderHost a -> IO a
runCocos2dHost cocos2d = do
    rec let runWithActions dm = runSpiderHost $ do
                va <- fireEventsAndRead dm $ sequence =<< readEvent voidActionHandle
                runHostFrame $ sequence_ va
        (result, voidActionHandle) <- runSpiderHost $ do
            (r, Cocos2dState vas) <- runStateT (runReaderT (unCocos2dHost cocos2d) (Cocos2dEnv runWithActions)) (Cocos2dState [])
            handle <- subscribeEvent $ mergeWith (>>) vas
            return $ (r, handle)
    return result

-- construct a new scene with a NodeGraph
scene :: (MonadReflexHost t m, MonadIO m) => Graph t () -> Cocos2dHost t m Scene
scene graph = do
    scene <- liftIO createScene
    runWithActions <- askRunWithActions
    voidAction <- runHostFrame $ do
        GraphState postBuild vas <- execStateT (runReaderT (unGraph graph) (GraphEnv (toNode scene) runWithActions)) (GraphState (return ()) [])
        postBuild
        return $ mergeWith (>>) vas
    addVoidAction voidAction
    return scene
