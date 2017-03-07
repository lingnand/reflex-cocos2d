{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Internal
    (
      SpiderNodeBuilder
    , mainScene
    )
  where

import Data.Dependent.Sum ((==>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Ref

import Reflex
import Reflex.Host.Class

import Foreign.Ptr (castPtr)
import Graphics.UI.Cocos2d (Decodable(..), CppPtr(..))
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Scene
import Graphics.UI.Cocos2d.Director

import Reflex.Cocos2d.Builder.Base
import Reflex.Cocos2d.Finalize.Base

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


type SpiderNodeBuilder = PostBuildT Spider (FinalizeT Spider IO (ImmediateNodeBuilderT Spider (PerformEventT Spider (SpiderHost Global))))

-- | Construct a new scene with a NodeBuilder
mainScene :: SpiderNodeBuilder a -> IO a
mainScene bd = do
    scene <- scene_create
    dtor <- director_getInstance
    winSize <- decode =<< director_getWinSize dtor
    (result, fins) <- runSpiderHost $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      rec let fireEvent ds = void . runSpiderHost $ fire ds (return ())
          ticks <- newEventWithTrigger $ \tr -> liftIO $ do
            sch <- director_getScheduler dtor
            let target = castPtr $ toPtr dtor
            scheduler_scheduleWithInterval sch
              (\ss -> fireEvent [tr ==> ss])
              target 0 False "ticks"
            return $ scheduler_unschedule sch "ticks" target
          let env = NodeBuilderEnv
                  { parent = toNode scene
                  , windowSize = winSize
                  , frameTicks = ticks
                  , fireEvent = fireEvent
                  }
          (resultWithFin, FireCommand fire) <- hostPerformEventT $
              flip runImmediateNodeBuilderT env $
              flip runFinalizeT (return ()) $
              runPostBuildT bd postBuild
      -- fire the post build event
      readRef postBuildTriggerRef >>= mapM_ (\t -> fire [t ==> ()] $ return ())
      return resultWithFin
    director_getInstance >>= flip director_runWithScene scene
    fins
    return result
