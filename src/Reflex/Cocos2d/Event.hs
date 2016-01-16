{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Reflex.Cocos2d.Event
    (
      FixEvent(..)
    -- * UI Events
    , UIEventType
    -- ** Convenience Lens Getters
    , touchesBegan
    , touchesEnded
    , touchesMoved
    , touchesCancelled
    , mouseDown
    , mouseUp
    , mouseMove
    , mouseScroll
    , keyPressed
    , keyReleased
    , accelerationChanged
    , uiEvents
    , dynKeysDown
    -- * Time
    , ticks
    , slowdown
    -- * Async
    , load
    -- * Utility
    , takeWhileE
    -- * re-export the lower level
    , Touch(..)
    , location
    , previousLocation
    , delta
    , startLocation
    , Key(..)
    , MouseEvent(..)
    , Acceleration(..)
    , vec
    , time
    ) where

import Data.Time.Clock
import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Control.Monad
import Control.Monad.Fix
import Control.Lens
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Schedule
import qualified JavaScript.Cocos2d.Async as A
import Reflex.Cocos2d.Class

-- | Recursive data type over Event
newtype FixEvent t f = FixEvent { unfixEvent :: f (Event t (FixEvent t f)) }

data UIEventType a where
    TouchesBegan :: UIEventType [Touch]
    TouchesEnded :: UIEventType [Touch]
    TouchesMoved :: UIEventType [Touch]
    TouchesCancelled :: UIEventType [Touch]
    MouseDown :: UIEventType MouseEvent
    MouseUp :: UIEventType MouseEvent
    MouseMove :: UIEventType MouseEvent
    MouseScroll :: UIEventType MouseEvent
    KeyPressed :: UIEventType Key
    KeyReleased :: UIEventType Key
    AccelerationChanged :: UIEventType Acceleration

touchesBegan, touchesEnded, touchesMoved, touchesCancelled
    :: IndexPreservingGetter (EventSelector t UIEventType) (Event t [Touch])
touchesBegan = to (select ?? TouchesBegan)
touchesEnded = to (select ?? TouchesEnded)
touchesMoved = to (select ?? TouchesMoved)
touchesCancelled = to (select ?? TouchesCancelled)

mouseDown, mouseUp, mouseMove, mouseScroll
    :: IndexPreservingGetter (EventSelector t UIEventType) (Event t MouseEvent)
mouseDown = to (select ?? MouseDown)
mouseUp = to (select ?? MouseUp)
mouseMove = to (select ?? MouseMove)
mouseScroll = to (select ?? MouseScroll)

keyPressed, keyReleased
    :: IndexPreservingGetter (EventSelector t UIEventType) (Event t Key)
keyPressed = to (select ?? KeyPressed)
keyReleased = to (select ?? KeyReleased)

accelerationChanged :: IndexPreservingGetter (EventSelector t UIEventType) (Event t Acceleration)
accelerationChanged = to (select ?? AccelerationChanged)

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

-- | Convenience function to obtain currently held keys
dynKeysDown :: (Reflex t, MonadHold t m, MonadFix m) => Event t Key -> Event t Key -> m (Dynamic t (S.Set Key))
dynKeysDown keyPressed keyReleased = foldDynMaybe ($) S.empty $ leftmost [ ffor keyPressed $ \k m -> do
                                                                              -- since KeyPressed event can keep firing
                                                                              -- we block non-changing events
                                                                              guard . not $ k `S.member` m
                                                                              return $ S.insert k m
                                                                         , ffor keyReleased $ fmap Just . S.delete
                                                                         ]

-- | Get the tick per frame
ticks :: NodeGraph t m => m (Event t NominalDiffTime)
ticks = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
        scheduleUpdate 0 $ \d -> runWithActions [et :=> d]

-- | Slow down a tick by a multiplier
slowdown :: (Reflex t, MonadHold t m, MonadFix m) => Int -> Event t NominalDiffTime -> m (Event t NominalDiffTime)
slowdown multiplier evt = do
    let incre dt (t, c) = (t+dt, c+1)
        t0 = fromInteger 0
    acc <- foldDyn incre (t0, 0) evt
    let succ = fforMaybe (updated acc) $ \(d, c) -> guard (c `rem` multiplier == 0)
                                                 >> return d
    dyn <- holdDyn t0 succ
    return $ attachWith subtract (current dyn) (updated dyn)

-- | Efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (f a) >> return never
    return . switch =<< hold e =<< headE e'

-- | Load a list of resources in an async manner
-- returns (Event t (loaded, total), finished)
-- NOTE: we reverse the @loaded@ and @total@ because this makes more sense
-- also, increment the finished by 1 because we are not procedurally
-- looking at how many loaded /last time/ (instead how many /already/
-- loaded)
load :: (NodeGraph t m, Num a) => [String] -> m (Event t (a, a), Event t ())
load resources = do
    o <- A.createLoadOption
    runWithActions <- askRunWithActions
    trigger <- newEventWithTrigger $ \et ->
        A.setLoadTrigger o $ \total loaded ->
            runWithActions [et :=> (fromIntegral (loaded+1), fromIntegral total)]
    finished <- newEventWithTrigger $ \et ->
        A.setLoadFinish o $ runWithActions [et :=> ()]
    schedulePostBuild $ A.load resources o
    return (trigger, finished)
