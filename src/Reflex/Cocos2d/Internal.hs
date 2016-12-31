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
    -- , NodeBuilder
    -- , runNodeBuilder
      mainScene

    )
  where

import Data.Dependent.Sum ((==>))
import Data.Functor.Contravariant
import Data.IORef
import Data.Dependent.Sum (DSum (..))
import Diagrams (V2)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Monad.Trans.Free
import Control.Lens
import Reflex
import Reflex.Host.Class

import Foreign.Ptr (castPtr)
import Foreign.Hoppy.Runtime (Decodable(..), CppPtr(..))

import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Scene
import Graphics.UI.Cocos2d.Director

import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes hiding (get)

-- mostly borrowed from Reflex.Dom.Internal
data BuilderState t m = BuilderState
    { _builderVoidActions :: ![Event t (m ())]
    , _builderFinalizers  :: m ()
    }

emptyBuilderState :: forall t m. Monad m => BuilderState t m
emptyBuilderState = BuilderState [] (return ())

builderVoidActions ::
  forall t m.
  Lens (BuilderState t m) (BuilderState t m) [Event t (m ())] [Event t (m ())]
builderVoidActions f (BuilderState act fin)
  = fmap (\ act' -> BuilderState act' fin) (f act)
{-# INLINE builderVoidActions #-}
builderFinalizers ::
  forall t m.
  Lens (BuilderState t m) (BuilderState t m) (m ()) (m ())
builderFinalizers f (BuilderState act fin)
  = fmap (\ fin' -> BuilderState act fin') (f fin)
{-# INLINE builderFinalizers #-}

newtype Builder t m a = Builder (ReaderT (NodeBuilderEnv t) (StateT (BuilderState t m) m) a)
    deriving ( Monad, Functor, Applicative
             , MonadReader (NodeBuilderEnv t)
             , MonadFix, MonadIO
             , MonadException, MonadAsyncException
             , MonadSample t, MonadHold t
             , MonadReflexCreateTrigger t, MonadSubscribeEvent t )

instance MonadTrans (Builder t) where
    lift = Builder . lift . lift

instance MonadRef m => MonadRef (Builder t m) where
    type Ref (Builder t m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

-- run builder with a given env and empty state
runBuilder :: Monad m
           => Builder t m a -> NodeBuilderEnv t -> m (a, BuilderState t m)
runBuilder (Builder builder) env =
    runStateT (runReaderT builder env) emptyBuilderState

instance {-# OVERLAPPING #-}
    (Reflex t, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m, MonadIO m)
    => EventSequencer t m (Builder t m) where
    seqEvent_ e = Builder $ builderVoidActions %= (e:)
    seqEventMaybe e = do
      run <- view runWithActions
      (eResult, trigger) <- newEventWithTriggerRef
      forEvent_ e $ \o -> do
          o >>= \case
            Just x -> liftIO $ readRef trigger >>= mapM_ (\t -> run ([t ==> x], return ()))
            _ -> return ()
      return eResult

instance {-# INCOHERENT #-}
    (Reflex t, EventSequencer t m (Builder t m), MonadIO m)
    => EventSequencer t IO (Builder t m) where
    seqEvent_ = seqEvent_ . fmap (liftIO :: IO a -> m a)
    seqEventMaybe = seqEventMaybe . fmap (liftIO :: IO a -> m a)

instance ( Reflex t, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m
         , MonadIO m, MonadFix m, MonadHold t m )
        => EventSequencer t (Builder t m) (Builder t m) where
    seqEventMaybe e = do
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        let onNewChildBuilt :: Event t (m ()) -> (a, [Event t (m ())]) -> Maybe (Event t (m ()))
            onNewChildBuilt _ (_, []) = Nothing
            onNewChildBuilt acc (_, vas) = Just $ mergeWith (>>) (acc:reverse vas)
        seqEvent_ . switch =<< accumMaybe onNewChildBuilt (never :: Event t (m ())) newChildBuilt
        builderEnv <- ask
        let run = builderEnv ^. runWithActions
        forEvent_ e $ \bd -> do
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
            (r, builderState) <- runBuilder bd (builderEnv & postBuildEvent .~ postBuildE)
            liftIO $ readRef newChildBuiltTriggerRef
                      >>= mapM_ (\t -> run ([t ==> (r, builderState^.builderVoidActions)], firePostBuild))
        return $ fmapMaybe fst newChildBuilt

instance ( Reflex t, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m
         , MonadIO m, MonadFix m, MonadHold t m )
        => EventSequenceHolder t (Builder t m) (Builder t m) where
    seqHold init e = do
        p <- asks $ view parent
        oldState <- Builder $ get <* put (BuilderState [] (return ()))
        result0 <- init
        state <- Builder $ get <* put oldState
        let voidAction0 = mergeWith (flip (>>)) (state^.builderVoidActions)
        (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
        seqEvent_ <=< switchPromptly voidAction0 $
              mergeWith (flip (>>)) . view (_2.builderVoidActions) <$> newChildBuilt
        finalizerBeh <- hold (state^.builderFinalizers) (view (_2.builderFinalizers) <$> newChildBuilt)
        builderEnv <- ask
        let run = builderEnv ^. runWithActions
        forEvent_ e $ \bd -> do
            finalizer <- sample finalizerBeh
            finalizer
            (postBuildE, postBuildTr) <- newEventWithTriggerRef
            let firePostBuild = readRef postBuildTr >>= mapM_ (\t -> run ([t ==> ()], return ()))
            (r, builderState) <- runBuilder bd $
                builderEnv & parent .~ p
                           & postBuildEvent .~ postBuildE
            liftIO $ readRef newChildBuiltTriggerRef
                    >>= mapM_ (\t -> run ([t ==> (r, builderState)], firePostBuild))
        return (result0, fst <$> newChildBuilt)
    seqDyn d = do
        (_, e) <- seqHold (return ()) =<< postponeCurrent d
        return e

instance
  ( Reflex t
  , MonadReflexCreateTrigger t host, MonadSubscribeEvent t host
  , MonadSample t host, MonadHold t host, MonadFix host
  , MonadRef host, Ref host ~ Ref IO
  , MonadIO host
  ) => NodeBuilder t host (Builder t host) where
    addFinalizer a = Builder $ builderFinalizers %= (a >>)

-- | Construct a new scene with a NodeBuilder
mainScene :: Builder Spider (HostFrame Spider) a -> IO a
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
        (result, builderState)
          <- runHostFrame . runBuilder bd $ NodeBuilderEnv (toNode scene) winSize postBuildE ticks runWithActions
        voidActionHandle <- subscribeEvent . mergeWith (flip (>>)) $ builderState^.builderVoidActions
        liftIO $ readRef postBuildTr >>= mapM_ (\t -> runWithActions ([t ==> ()], return ()))
        return result
    director_getInstance >>= flip director_runWithScene scene
    return result

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
-- dyn' :: ( BuilderHostFrame t m
--         , IsSettable w (HostFrame t) a (attr w (HostFrame t) b a)
--         , IsSettable w m a (attr w m b a) )
--      => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
--      -> WOAttrib' w m (Dynamic t a)
-- dyn' attr = WOAttrib $ \w d -> do
--               setter attr w =<< sample (current d)
--               let WOAttrib es = evt attr
--               es w (updated d)
-- uDyn' :: ( BuilderHostFrame t m
--          , IsSettable w (HostFrame t) a (attr w (HostFrame t) b a)
--          , IsSettable w m a (attr w m b a)
--          , Eq a )
--       => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
--       -> WOAttrib' w m (UniqDynamic t a)
-- uDyn' attr = fromUniqDynamic >$< dyn' attr
