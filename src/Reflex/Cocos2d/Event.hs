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
    -- * Async
    , load
    -- * Utility
    , mapAccumMaybe
    , mapAccum
    , accum
    , dilate
    , takeWhileE
    , dropWhileE
    , breakE
    -- * re-export the lower level
    , Touch(..)
    , loc
    , previousLoc
    , delta
    , startLoc
    , Key(..)
    , MouseEvent
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
          TouchesBegan -> wrap createTouchAllAtOnceEventListener $ setOnTouchesBegan ?? \t -> runWithActions [et :=> Identity t]
          TouchesEnded -> wrap createTouchAllAtOnceEventListener $ setOnTouchesEnded ?? \t -> runWithActions [et :=> Identity t]
          TouchesMoved -> wrap createTouchAllAtOnceEventListener $ setOnTouchesMoved ?? \t -> runWithActions [et :=> Identity t]
          TouchesCancelled -> wrap createTouchAllAtOnceEventListener $ setOnTouchesCancelled ?? \t -> runWithActions [et :=> Identity t]
          MouseDown -> wrap createMouseEventListener $ setOnMouseDown ?? \m -> runWithActions [et :=> Identity m]
          MouseUp -> wrap createMouseEventListener $ setOnMouseUp ?? \m -> runWithActions [et :=> Identity m]
          MouseMove -> wrap createMouseEventListener $ setOnMouseMove ?? \m -> runWithActions [et :=> Identity m]
          MouseScroll -> wrap createMouseEventListener $ setOnMouseScroll ?? \m -> runWithActions [et :=> Identity m]
          KeyPressed -> wrap createKeyboardEventListener $ setOnKeyPressed ?? \k -> runWithActions [et :=> Identity k]
          KeyReleased -> wrap createKeyboardEventListener $ setOnKeyReleased ?? \k -> runWithActions [et :=> Identity k]
          AccelerationChanged -> wrap createAccelerationEventListener $ setOnAccelerationEvent ?? \acc -> runWithActions [et :=> Identity acc]
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
        scheduleUpdate 0 $ \d -> runWithActions [et :=> Identity d]

mapAccumMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe f z e = do
    e' <- foldDyn (\b (a, _) -> f a b) (z, Nothing) e
    return . fmapMaybe snd $ updated e'

mapAccum :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum f = mapAccumMaybe $ \a b -> let (a', c) = f a b in (a', Just c)

accum :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Event t (a, b))
accum f = mapAccum $ \a b -> let a' = f a b in (a', (a', b))

-- | Modulate a ticker
dilate :: (Reflex t, MonadHold t m, MonadFix m, Num a, Ord a) => a -> Event t a -> m (Event t a)
dilate limit = flip mapAccumMaybe (0, limit) $ \(acc, l) d ->
                    let sum = acc + d in
                    if sum > l then ((0, limit-(sum-l)), Just sum)
                               else ((sum, l), Nothing)

-- | Efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    return . switch =<< hold e' =<< headE (const never <$> gateE)

-- | Efficiently cut off a stream of events at a point
dropWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
dropWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (not $ f a) >> return e
    switchPromptly never =<< headE e'

-- | split an Event into two parts on a condition
breakE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a, Event t a)
breakE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    gateE' <- headE gateE
    bef <- switch <$> hold e' (const never <$> gateE')
    aft <- switchPromptly never (const e <$> gateE')
    return (bef, aft)


-- | split an Event into two parts by counting the numbers
-- splitE ::

-- takeE
--
-- dropE
--
--

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
            runWithActions [et :=> Identity (fromIntegral (loaded+1), fromIntegral total)]
    finished <- newEventWithTrigger $ \et ->
        A.setLoadFinish o $ runWithActions [et :=> Identity ()]
    schedulePostBuild $ A.load resources o
    return (trigger, finished)

