module Reflex.Cocos2d.Audio
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
    )
  where

import Data.Dependent.Sum ((==>))
import Control.Monad.Trans
import Control.Lens

import Graphics.UI.Cocos2d.Audio

import Reflex
import Reflex.Host.Class
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Internal
import Reflex.Cocos2d.Attributes

data AudioStateCommand = AudioState_Play | AudioState_Pause | AudioState_Stop deriving (Show, Read)

type AudioInstance = Int

playAudio :: MonadIO m => String -> [Prop AudioInstance m] -> m AudioInstance
playAudio filename props = do
    au <- liftIO $ audioEngine_play2d filename False 1.0
    setProps au props
    return au

getAudioFinishedEvent :: MonadReflexCreateTrigger t m => AudioInstance -> NodeBuilder t m (Event t ())
getAudioFinishedEvent id = do
    run <- view runWithActions
    newEventWithTrigger $ \tr -> do
      audioEngine_setFinishCallback id $ \_ _ -> run ([tr ==> ()], return ())
      return $ pure ()

preloadAudio :: MonadReflexCreateTrigger t m => String -> NodeBuilder t m (Event t Bool)
preloadAudio filename = do
    run <- view runWithActions
    newEventWithTrigger $ \tr -> do
      audioEngine_preloadWithCallback filename $ \success -> run ([tr ==> success], return ())
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
