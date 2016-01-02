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

data GraphState t m = GraphState { _graphPostBuild :: !(m ())
                                 , _graphVoidActions :: ![Event t (m ())]
                                 }

makeLenses ''GraphState

newtype Graph t m a = Graph { unGraph :: ReaderT (GraphEnv t) (StateT (GraphState t m) m) a }
        deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance MonadTrans (Graph t) where
    lift = Graph . lift . lift

instance MonadSample t m => MonadSample t (Graph t m) where
    sample b = lift $ sample b

instance MonadHold t m => MonadHold t (Graph t m) where
    hold v0 e = lift $ hold v0 e

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Graph t m) where
    newEventWithTrigger = lift . newEventWithTrigger
    newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance ( MonadIO m, MonadAsyncException m, Functor m
         , ReflexHost t, MonadReflexCreateTrigger t m, MonadSample t m, MonadHold t m
         , MonadFix m) => NodeGraph t (Graph t m) where
    type GraphHost (Graph t m) = m
    askParent = Graph $ view graphParent
    schedulePostBuild a = Graph $ graphPostBuild %= (a>>)
    addVoidAction a = Graph $ graphVoidActions %= (a:)
    subGraph n child = Graph $ local (graphParent .~ n) $ unGraph child
    askRunWithActions = Graph $ view graphRunWithActions 

data Cocos2dEnv = Cocos2dEnv { _cocos2dRunWithActions :: !(ActionTrigger Spider) }

makeLenses ''Cocos2dEnv

data Cocos2dState = Cocos2dState { _cocos2dVoidActions :: ![Event Spider (HostFrame Spider ())] }
makeLenses ''Cocos2dState

type Cocos2dHost a = ReaderT Cocos2dEnv (StateT Cocos2dState SpiderHost) a

runCocos2dHost :: Cocos2dHost a -> IO a
runCocos2dHost cocos2d = do
    rec let runWithActions dm = runSpiderHost $ do
                va <- fireEventsAndRead dm $ sequence =<< readEvent voidActionHandle
                runHostFrame $ sequence_ va
        (result, voidActionHandle) <- runSpiderHost $ do
            (r, Cocos2dState vas) <- runStateT (runReaderT cocos2d (Cocos2dEnv runWithActions)) (Cocos2dState [])
            handle <- subscribeEvent $ mergeWith (>>) vas
            return $ (r, handle)
    return result

-- construct a new scene with a NodeGraph
scene :: Graph Spider (HostFrame Spider) () -> Cocos2dHost Scene
scene graph = do
    scene <- liftIO createScene
    runWithActions <- view cocos2dRunWithActions
    voidAction <- lift . lift . runHostFrame $ do
        GraphState postBuild vas <- execStateT (runReaderT (unGraph graph) (GraphEnv (toNode scene) runWithActions)) (GraphState (return ()) [])
        postBuild
        return $ mergeWith (>>) vas
    cocos2dVoidActions %= (voidAction:)
    return scene
