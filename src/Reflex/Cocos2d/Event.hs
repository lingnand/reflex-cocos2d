{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Reflex.Cocos2d.Event where

import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Lens ((??))
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.EventListener
import JavaScript.Cocos2d.Types
import Reflex.Cocos2d.Class

data EventType a where
    TouchesBegan :: EventType [Touch]
    TouchesEnded :: EventType [Touch]
    TouchesMoved :: EventType [Touch]
    TouchesCancelled :: EventType [Touch]
    MouseDown :: EventType MouseEvent
    MouseUp :: EventType MouseEvent
    MouseMove :: EventType MouseEvent
    MouseScroll :: EventType MouseEvent
    KeyPressed :: EventType Key -- | similar to KeyDown
    KeyReleased :: EventType Key -- | similar to KeyUp
    AccelerationChanged :: EventType Acceleration

deriveGEq ''EventType
deriveGCompare ''EventType

globalEvents :: MonadCocos2dHost t m => m (EventSelector t EventType)
globalEvents = do
    runWithActions <- askRunWithActions
    e <- newFanEventWithTrigger $ \eventType et ->
        let wrap ml cb = do
                l <- ml
                releaseCb <- cb l
                -- put the listener to priority > scene graph
                addListener' l (-1)
                return $ removeListener l >> releaseCb
        in case eventType of
            TouchesBegan -> wrap createTouchAllAtOnceEventListener $ setOnTouchesBegan ?? \t -> runWithActions [et :=> t]
            TouchesEnded -> wrap createTouchAllAtOnceEventListener $ setOnTouchesEnded ?? \t -> runWithActions [et :=> t]
            TouchesMoved -> wrap createTouchAllAtOnceEventListener $ setOnTouchesMoved ?? \t -> runWithActions [et :=> t]
            TouchesCancelled -> wrap createTouchAllAtOnceEventListener $ setOnTouchesCancelled ?? \t -> runWithActions [et :=> t]
            MouseDown -> wrap createMouseEventListener $ setOnMouseDown ?? \m -> runWithActions [et :=> m]
            MouseUp -> wrap createMouseEventListener $ setOnMouseUp ?? \m -> runWithActions [et :=> m]
            MouseMove -> wrap createMouseEventListener $ setOnMouseMove ?? \m -> runWithActions [et :=> m]
            MouseScroll -> wrap createMouseEventListener $ setOnMouseScroll ?? \m -> runWithActions [et :=> m]
            KeyPressed -> wrap createKeyboardEventListener $ setOnKeyPressed ?? \k -> runWithActions [et :=> k]
            KeyReleased -> wrap createKeyboardEventListener $ setOnKeyReleased ?? \k -> runWithActions [et :=> k]
            AccelerationChanged -> wrap createAccelerationEventListener $ setOnAccelerationEvent ?? \acc -> runWithActions [et :=> acc]
    return $! e

--- convenience function to obtain currently held keys
dynKeymap :: (Reflex t, MonadHold t m, MonadFix m) => Event t Key -> Event t Key -> m (Dynamic t (S.Set Key))
dynKeymap keydown keyup = foldDyn ($) S.empty $ leftmost [ S.insert <$> keydown
                                                         , S.delete <$> keyup ]
