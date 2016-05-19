{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Reflex.Cocos2d.Event
    (
    -- * UI Events
      UIEventType
    -- ** Convenience Lens Getters
    , TouchEvents(TouchEvents)
    , HasTouchEvents(..)
    , SingleTouchEvents(SingleTouchEvents)
    , HasSingleTouchEvents(..)
    , DragEvent
    , dragBegan
    , dragMoved
    , dragEnded
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
    , dilate
    -- , ticks'
    , delay'
    -- * Async
    , load
    -- * Rand
    , picks
    -- * Utility
    , nodeContains
    , mapAccumMaybe
    , mapAccum
    , accum
    , takeWhileE
    , dropWhileE
    , breakE
    , stack
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

import Diagrams (P2)
import Diagrams.BoundingBox
import Data.Time.Clock
import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Lens hiding (contains)
import Math.Probable hiding (Event, never)
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Schedule
import qualified JavaScript.Cocos2d.Async as A
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Node
import Linear.Affine
import Control.Monad.Ref

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

nodeContains :: (MonadIO m, MonadSample t m, IsNode n, HasSizeConfig n t) => n -> (P2 Double) -> m Bool
nodeContains n p = do
    sz <- sample (current $ n^.size)
    p' <- convertToNodeSpace n p
    return $ fromCorners 0 (0.+^sz) `contains` p'

-- a datatype representing the necessary information regarding a drag event
data DragEvent t = DragEvent { _dragBegan :: Touch
                             , _dragMoved :: Event t Touch
                             , _dragEnded :: Event t Touch
                             }
makeLenses ''DragEvent

-- return Event (dragStart, Event dragging, Event dragEnd)
dragged :: NodeGraph t m => SingleTouchEvents t -> m (Event t (DragEvent t))
dragged (SingleTouchEvents began moved ended _) = do
    let mstream = leftmost [ (True,) <$> moved
                           , (False,) <$> ended
                           ]
    rec e' <- forG began $ \t -> do
          (ms, _) <- sample b'
          (movedSeg, ms') <- breakE fst ms
          (endedSeg, ms'') <- headTailE ms'
          return (ms'', Just $ DragEvent t (snd <$> movedSeg) (snd <$> endedSeg))
        b' <- hold (mstream, Nothing) e'
    return $ fmapMaybe snd e'

-- | Modulate a ticker
dilate :: (Reflex t, MonadHold t m, MonadFix m, Num a, Ord a) => a -> Event t a -> m (Event t a)
dilate limit = flip mapAccumMaybe (0, limit) $ \(acc, l) d ->
                    let sum = acc + d in
                    if sum > l then ((0, limit-(sum-l)), Just sum)
                               else ((sum, l), Nothing)

-- | Get the tick per frame
ticks :: NodeGraph t m => m (Event t NominalDiffTime)
ticks = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
        scheduleUpdate 0 $ \d -> runWithActions [et :=> Identity d]

-- ticks' :: NodeGraph t m => NominalDiffTime -> m (Event t NominalDiffTime)
-- ticks' interval = do
--     runWithActions <- askRunWithActions
--     newEventWithTrigger $ \et ->
--         scheduleUpdate' interval True $ \d -> runWithActions [et :=> Identity d]

-- | Sample a value out of each RandT on each new event
picks :: NodeGraph t m => Event t (RandT m a) -> RandT m (Event t a)
picks evt = RandT $ \g -> forG evt $ \rt -> runRandT rt g

-- | Delay an Event by the given amount of time
delay' :: NodeGraph t m => NominalDiffTime -> Event t a -> m (Event t a)
delay' dt e = do
    runWithActions <- askRunWithActions
    (e', trigger) <- newEventWithTriggerRef
    sequenceH_ . ffor e $ \a -> mdo
      -- ignoring the tear-down function for now
      release <- scheduleUpdate' dt False $ \_ -> do
        readRef trigger >>= mapM_ (\t -> runWithActions [t :=> Identity a])
        -- after finished with the update, release directly
        release
      return ()
    return e'

mapAccumMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe f z e = do
    e' <- foldDyn (\b (a, _) -> f a b) (z, Nothing) e
    return . fmapMaybe snd $ updated e'

mapAccum :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum f = mapAccumMaybe $ \a b -> let (a', c) = f a b in (a', Just c)

accum :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Event t (a, b))
accum f = mapAccum $ \a b -> let a' = f a b in (a', (a', b))

-- | Efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    return . switch =<< hold e' =<< headE (never <$ gateE)

-- | Efficiently cut off a stream of events at a point
dropWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
dropWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (not $ f a) >> return e
    switchPromptly never =<< headE e'

-- | Split an Event into two parts on a condition
breakE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a, Event t a)
breakE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    gateE' <- headE gateE
    bef <- switch <$> hold e' (never <$ gateE')
    aft <- switchPromptly never (e <$ gateE')
    return (bef, aft)

-- | Simple stack that responds to Events
stack :: (Reflex t, MonadHold t m, MonadFix m)
      => Event t (a -> b) -- ^ the request Event for taking items out / popping
      -> [a] -- ^ the initial state
      -> Event t a -- ^ the input states
      -> m (Dynamic t [a], Event t (Maybe b)) -- ^ (stack Dynamic, the output Event)
stack reqs z input = do
    rec let e = mergeWith (\f1 f2 a -> let (a', e1)  = f1 a
                                           (a'', e2) = f2 (fromMaybe a a')
                                       in (a'' <|> a', e2 <|> e1)
                          )
                [ (\i a -> (Just (i:a), Nothing)) <$> input
                , ffor reqs $ \f -> \case
                    [] -> (Nothing, Just Nothing) -- if there is nothing to take out, we get Nothing
                    (a:as) -> (Just as, Just . Just $ f a)
                ]
            e' = push ?? e $ \f -> do
                      a <- sample b'
                      return . Just $ f a
            se' = fmapMaybe fst e'
        b' <- hold z se'
    return (unsafeDynamic b' se', fmapMaybe snd e')


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

