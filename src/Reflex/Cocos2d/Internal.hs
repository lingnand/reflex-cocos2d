{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Internal
  (
    SpiderNodeBuilder
  , mainScene
  ) where

import Data.Coerce
import Data.Sequence
import Data.Unique.Tag
import Data.Functor.Misc
import Data.Semigroup
import Data.Foldable
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.State.Strict

import Reflex
import Reflex.Spider.Internal (SpiderHostFrame)
import Reflex.Host.Class

import Foreign.Ptr (castPtr)
import Graphics.UI.Cocos2d (Decodable(..), CppPtr(..))
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Scene
import Graphics.UI.Cocos2d.Director

import Reflex.Cocos2d.Builder.Base
import Reflex.Cocos2d.Finalize.Base
import Reflex.Cocos2d.Accum.Class
import Reflex.Cocos2d.Internal.Global (globalDirector, globalScheduler)

-- NOTE: some notes
--
-- to use hostPerformEventT, note that we need to make sure that the actions triggered by the FireCommand DO NOT trigger FireCommand directly again - rather, they should append their TriggerInvocations to some list before the actual firing comes about again...
-- for async actions, the 'trigger' actions (a -> IO ()) should post and fire the value via
-- runInCocos such that the population is done inside the main thread
-- so hopefully, my ACTUAL trigger function is something like this:
-- if async
-- then postToCocos action
-- else action
--  where action = do
--    check variable if the processing is running -- note no lock is needed as it's always in the
--                                                   same thread...?
--    if yes, then append to a list
--    else, call FireCommand with checking
-- WAIT A MOMENT:
-- a trigger that triggers more triggers could ONLY happen when the trigger involves some impure
-- actions that run the fire command. If we NEVER ALLOW THAT TO HAPPEN, and for each impure action
-- that needs to return a response to go through the Requesting framework, then we don't need the
-- hacky variable route anymore
--
-- In case where we still need to interact with a trigger function directly e.g., from an async
-- call, we should wrap that in a runInCocos so that it STILL doesn't need any hacky variable.

-- type NodeBuilder = PostBuildT Spider ()

type Finalizer = SpiderHostFrame Global
type SpiderNodeBuilder = PostBuildT Spider (FinalizeT Spider Finalizer (ImmediateNodeBuilderT Spider (PerformEventT Spider (SpiderHost Global))))

-- so that we can lift the finalizer through to m
instance MonadIO m => MonadRun Finalizer m where
    -- XXX: Finalizer *cannot* run directly in PerformEvent because:
    -- During runWithReplace, the new replacement actions are performed via
    -- normal requests, which go via the same route as all other
    -- performEvent_ actions in the live scene. When the replacement is
    -- actually triggered, it's possible those other performEvent_ actions
    -- are triggered in the same frame as well - so if the finalizer is run
    -- directly, a segfault occurs. As safety measure we lift the Finalizer
    -- type only by running it manually at the end of a cocos loop
    run fin = liftIO $
        -- NOTE: we have to make sure the triggering is performed in the main thread
        scheduler_performFunctionInCocosThread globalScheduler $
          runSpiderHost $ runHostFrame fin

-- XXX: why do we have to fix the monad to SpiderHost Global...?
instance (ReflexHost t, PrimMonad (HostFrame t)) => MonadAccum t (PerformEventT t (SpiderHost Global)) where
    runWithAccum outerA0 outerA' = PerformEventT $ runWithAccumRequesterTWith f (coerce outerA0) (coerceEvent outerA')
      where f :: PrimMonad (HostFrame t) => HostFrame t a -> Event t (HostFrame t b) -> RequesterT t (HostFrame t) Identity (HostFrame t) (a, Event t b)
            f a0 a' = do
              result0 <- lift a0
              result' <- requestingIdentity a'
              return (result0, result')

-- XXX: duplicated functions to achieve accumulation
runWithAccumRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m, MonadFix m)
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithAccumRequesterTWith f a0 a' =
  let f' :: forall a' b'. ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m a'
         -> Event t (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m b')
         -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (a', Event t b')
      f' x y = do
        r <- EventWriterT ask
        unRequesterT (f (runReaderT x r) (fmapCheap (`runReaderT` r) y))
  in RequesterT $ runWithAccumEventWriterTWith f' (coerce a0) (coerceEvent a')

runWithAccumEventWriterTWith :: forall m t w a b. (Reflex t, MonadHold t m, MonadFix m, Semigroup w)
                               => (forall a' b'. m a' -> Event t (m b') -> EventWriterT t w m (a', Event t b'))
                               -> EventWriterT t w m a
                               -> Event t (EventWriterT t w m b)
                               -> EventWriterT t w m (a, Event t b)
runWithAccumEventWriterTWith f a0 a' = do
  let g :: EventWriterT t w m c -> m (c, Seq (Event t w))
      g (EventWriterT r) = runStateT r mempty
      combine :: Seq (Event t w) -> Event t w
      combine = fmapCheap sconcat . mergeList . toList
  (result0, result') <- f (g a0) $ fmap g a'
  request <- accum (<>) (combine $ snd result0) $ fmapCheap (combine . snd) result'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  -- TODO: test if coincidence leads to nasty loop because we are
  -- recombining the old event into the new event
  EventWriterT $ modify $ flip (|>) $ coincidence $ updated request
  EventWriterT $ modify $ flip (|>) $ switch $ current request
  return (fst result0, fmapCheap fst result')

-- | Construct a new scene with a NodeBuilder
mainScene :: SpiderNodeBuilder a -> IO a
mainScene bd = do
    scene <- scene_create
    winSize <- decode =<< director_getWinSize globalDirector
    result <- runSpiderHost $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      rec let fireEvent ds = void . runSpiderHost $ fire ds (return ())
          ticks <- newEventWithTrigger $ \tr -> liftIO $ do
            let target = castPtr $ toPtr globalDirector
            scheduler_scheduleWithInterval globalScheduler
              (\ss -> fireEvent [tr ==> ss])
              target 0 False "ticks"
            return $ scheduler_unschedule globalScheduler "ticks" target
          let env = NodeBuilderEnv
                  { parent = toNode scene
                  , windowSize = winSize
                  , frameTicks = ticks
                  , fireEvent = fireEvent
                  }
          -- XXX: ignore the initial fins because we are not going to
          -- deallocate the entire game
          ((result, _), FireCommand fire) <- hostPerformEventT $
              flip runImmediateNodeBuilderT env $
              flip runFinalizeT (return ()) $
              runPostBuildT bd postBuild
      -- fire the post build event
      readRef postBuildTriggerRef >>= mapM_ (\t -> fire [t ==> ()] $ return ())
      return result
    director_runWithScene globalDirector scene
    return result
