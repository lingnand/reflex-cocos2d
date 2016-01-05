{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Reflex.Cocos2d.Event where

import Data.Time.Clock
import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Lens ((??))
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.EventListener
import JavaScript.Cocos2d.Schedule
import JavaScript.Cocos2d.Types
import Reflex.Cocos2d.Class

data UIEventType a where
    TouchesBegan :: UIEventType [Touch]
    TouchesEnded :: UIEventType [Touch]
    TouchesMoved :: UIEventType [Touch]
    TouchesCancelled :: UIEventType [Touch]
    MouseDown :: UIEventType MouseEvent
    MouseUp :: UIEventType MouseEvent
    MouseMove :: UIEventType MouseEvent
    MouseScroll :: UIEventType MouseEvent
    KeyPressed :: UIEventType Key -- | similar to KeyDown
    KeyReleased :: UIEventType Key -- | similar to KeyUp
    AccelerationChanged :: UIEventType Acceleration

deriveGEq ''UIEventType
deriveGCompare ''UIEventType

uiEvents :: NodeGraph t m => m (EventSelector t UIEventType)
uiEvents = do
    runWithActions <- askRunWithActions
    e <- newFanEventWithTrigger $ \uiEventType et ->
        let wrap ml cb = do
                l <- ml
                releaseCb <- cb l
                -- put the listener to priority > scene graph
                addListener' l (-1)
                return $ removeListener l >> releaseCb
        in case uiEventType of
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
--- TODO: use foldDynMaybe to reduce non-changing events?
dynKeysDown :: (Reflex t, MonadHold t m, MonadFix m) => Event t Key -> Event t Key -> m (Dynamic t (S.Set Key))
dynKeysDown keyPressed keyReleased = foldDyn ($) S.empty $ leftmost [ S.insert <$> keyPressed
                                                                    , S.delete <$> keyReleased ]

-- | get the tick per frame
ticks :: NodeGraph t m => m (Event t NominalDiffTime)
ticks = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
        scheduleUpdate 0 $ \d -> runWithActions [et :=> d]

-- | slow down a tick by a multiplier
slowdown :: (Reflex t, MonadHold t m, MonadFix m) => Int -> Event t NominalDiffTime -> m (Event t NominalDiffTime)
slowdown multiplier evt = do
    let incre dt (t, c) = (t+dt, c+1)
        t0 = fromInteger 0
    acc <- foldDyn incre (t0, 0) evt
    let succ = fforMaybe (updated acc) $ \(d, c) -> guard (c `rem` multiplier == 0)
                                                 >> return d
    dyn <- holdDyn t0 succ
    return $ attachWith subtract (current dyn) (updated dyn)

-- | efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (f a) >> return never
    return . switch =<< hold e =<< headE e'
