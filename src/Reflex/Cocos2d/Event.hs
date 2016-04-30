{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Reflex.Cocos2d.Event
    (
      FixEvent(..)
    -- * UI Events
    , UIEventType
    -- ** Convenience Lens Getters
    , TouchEvents(TouchEvents)
    , HasTouchEvents(..)
    , touched
    , dragged
    , mouseDown
    , mouseUp
    , mouseMove
    , mouseScroll
    , keyPressed
    , keyReleased
    , accelChanged
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
    , Accel(..)
    , time
    ) where

import Data.Time.Clock
import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Data.Maybe
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Maybe
import Control.Lens hiding (contains)
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Schedule
import qualified JavaScript.Cocos2d.Async as A
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Node
import Linear.Affine
import Diagrams.BoundingBox

-- | Recursive data type over Event
newtype FixEvent t f = FixEvent { unfixEvent :: f (Event t (FixEvent t f)) }

data TouchEvents t = TouchEvents { _touchesBegan :: Event t [Touch]
                                 , _touchesMoved :: Event t [Touch]
                                 , _touchesEnded :: Event t [Touch]
                                 , _touchesCancelled :: Event t [Touch]
                                 }

data SingleTouchEvents t = SingleTouchEvents { _touchBegan :: Event t Touch
                                             , _touchMoved :: Event t Touch
                                             , _touchEnded :: Event t Touch
                                             , _touchCancelled :: Event t Touch
                                             }

class HasTouchEvents c t | c -> t where
    touchEvents :: Getter c (TouchEvents t)
    touchesBegan :: Getter c (Event t [Touch])
    touchesBegan = touchEvents . to _touchesBegan
    touchesMoved :: Getter c (Event t [Touch])
    touchesMoved = touchEvents . to _touchesMoved
    touchesEnded :: Getter c (Event t [Touch])
    touchesEnded = touchEvents . to _touchesEnded
    touchesCancelled :: Getter c (Event t [Touch])
    touchesCancelled = touchEvents . to _touchesCancelled

class HasSingleTouchEvents c t | c -> t where
    singleTouchEvents :: Getter c (SingleTouchEvents t)
    touchBegan :: Getter c (Event t Touch)
    touchBegan = singleTouchEvents . to _touchBegan
    touchMoved :: Getter c (Event t Touch)
    touchMoved = singleTouchEvents . to _touchMoved
    touchEnded :: Getter c (Event t Touch)
    touchEnded = singleTouchEvents . to _touchEnded
    touchCancelled :: Getter c (Event t Touch)
    touchCancelled = singleTouchEvents . to _touchCancelled

filterSingular :: Reflex t => Event t [a] -> Event t a
filterSingular = fmapMaybe $ \case
                  [a] -> Just a
                  _ -> Nothing

instance (Reflex t, HasTouchEvents c t) => HasSingleTouchEvents c t where
    touchBegan = touchesBegan . to filterSingular
    touchMoved = touchesMoved . to filterSingular
    touchEnded = touchesEnded . to filterSingular
    touchCancelled = touchesCancelled . to filterSingular
    singleTouchEvents = to $ \c -> SingleTouchEvents { _touchBegan = c ^. touchBegan
                                                     , _touchMoved = c ^. touchMoved
                                                     , _touchEnded = c ^. touchEnded
                                                     , _touchCancelled = c ^. touchCancelled
                                                     }

instance HasTouchEvents (TouchEvents t) t where
    touchEvents = id


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
    AccelChanged :: UIEventType (Accel Double)

instance HasTouchEvents (EventSelector t UIEventType) t where
    touchEvents = to $ TouchEvents <$> (select ?? TouchesBegan)
                                   <*> (select ?? TouchesMoved)
                                   <*> (select ?? TouchesEnded)
                                   <*> (select ?? TouchesCancelled)
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

accelChanged :: IndexPreservingGetter (EventSelector t UIEventType) (Event t (Accel Double))
accelChanged = to (select ?? AccelChanged)

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
          AccelChanged -> do
            (l, releaseCb) <- createAccelEventListener (\acc -> runWithActions [et :=> Identity acc])
            addListener' l (-1)
            return $ removeListener l >> releaseCb
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

touched :: (NodeGraph t m, IsNode n, HasSizeConfig n t) => n -> Event t [Touch] -> m (Event t Touch)
touched node touchesBegan =
    forHMaybe (attachDyn (node^.size) touchesBegan) $ \(sz, touches) ->
      let box = fromCorners 0 (0.+^sz) in
      runMaybeT . msum . ffor touches $ \t -> do
        loc' <- convertToNodeSpace node $ t^.loc
        guard $ box `contains` loc'
        return t

data DragPhase = DragBegan | DragMoved | DragEnded
-- return (Event dragStart, Event dragging, Event dragEnd)
dragged :: (NodeGraph t m, IsNode n, HasSizeConfig n t) => n -> SingleTouchEvents t -> m (Event t Touch, Event t Touch, Event t Touch)
dragged n tevts = do
    dragBeganEvt <- touched n (tevts^.touchesBegan)
    -- TODO: search across the touches instead of only taking the first one?
    let dbegan = (DragBegan,) <$> dragBeganEvt
        dmove = fforMaybe (tevts^.touchesMoved) $ fmap (DragMoved,) . listToMaybe
        dend = fforMaybe (tevts^.touchesEnded) $ fmap (DragEnded,) . listToMaybe
        touches = leftmost [ dbegan , dmove , dend ]
    -- fold over the touches to get Event t (isBeingDragged, Position)
    dragMovedEvt' <- mapAccumMaybe ?? False ?? touches $ \beingDragged (ph, t) ->
        case (beingDragged, ph) of
          (False, DragBegan) -> (True, Just (True, t))
          (True, DragMoved) -> (True, Just (True, t))
          (True, DragEnded) -> (False, Just (False, t))
          _ -> (beingDragged, Nothing)
    let dragEndedEvt = fforMaybe dragMovedEvt' $ \(beingDragged, t) -> guard (not beingDragged) >> return t
        dragMovedEvt = fmap snd dragMovedEvt' -- TODO: should we take the head and last off?
    return (dragBeganEvt, dragMovedEvt, dragEndedEvt)

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
load :: (NodeGraph t m) => [String] -> m (Event t (Int, Int), Event t ())
load resources = do
    o <- A.createLoadOption
    runWithActions <- askRunWithActions
    trigger <- newEventWithTrigger $ \et ->
        A.setLoadTrigger o $ \total loaded ->
            runWithActions [et :=> Identity (loaded+1, total)]
    finished <- newEventWithTrigger $ \et ->
        A.setLoadFinish o $ runWithActions [et :=> Identity ()]
    schedulePostBuild $ A.load resources o
    return (trigger, finished)

