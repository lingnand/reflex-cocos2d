{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Misc.Audio
  ( AudioStateCommand(..)
  , AudioInstance
  , playAudio
  , getAudioFinishedEvent
  , preloadAudio
  -- attrs
  , audioState
  , loop
  , volume
  , currentTime
  ) where

import Control.Monad.Trans

import Graphics.UI.Cocos2d.Audio

import Reflex

import Reflex.Cocos2d.FastTriggerEvent.Class
import Reflex.Cocos2d.Attributes.Base

data AudioStateCommand = AudioState_Play | AudioState_Pause | AudioState_Stop deriving (Show, Read)

type AudioInstance = Int

playAudio :: MonadIO m => String -> [Prop AudioInstance m] -> m AudioInstance
playAudio filename props = do
    au <- liftIO $ audioEngine_play2d filename False 1.0
    setProps au props
    return au

getAudioFinishedEvent :: FastTriggerEvent t m => AudioInstance -> m (Event t ())
getAudioFinishedEvent id = fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
    audioEngine_setFinishCallback id $ \_ _ -> triggerFunc () (return ())
    return $ pure ()

preloadAudio :: FastTriggerEvent t m => String -> m (Event t Bool)
preloadAudio filename = fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
    audioEngine_preloadWithCallback filename $ \success -> triggerFunc success (return ())
    return $ pure ()

audioState :: MonadIO m => WOAttrib' AudioInstance m AudioStateCommand
audioState = WOAttrib $ \id cmd -> liftIO $ case cmd of
              AudioState_Play -> audioEngine_resume id
              AudioState_Pause -> audioEngine_pause id
              AudioState_Stop -> audioEngine_stop id

loop :: MonadIO m => Attrib' AudioInstance m Bool
loop = hoistA liftIO $ Attrib audioEngine_isLoop audioEngine_setLoop

volume :: MonadIO m => Attrib' AudioInstance m Float
volume = hoistA liftIO $ Attrib audioEngine_getVolume audioEngine_setVolume

currentTime :: MonadIO m => Attrib' AudioInstance m Float
currentTime = hoistA liftIO $ Attrib audioEngine_getCurrentTime audioEngine_setCurrentTime
