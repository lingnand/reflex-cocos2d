{-# LANGUAGE RankNTypes #-}
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
    -- * Rand
    , runRandEvent
    -- , ticks'
    , delay
    -- * Async
    , load
    -- * Utility
    , nodeContains
    -- * Reflex Utilities
    , switchF
    -- * Widget
    , WidgetTouchEvents(WidgetTouchEvents)
    , HasWidgetTouchEvents(..)
    , WidgetEvents(WidgetEvents)
    , widgetClicked
    , widgetEvents
    , pageViewEvents
    , listViewEvents
    , scrollViewEvents
    , sliderEvents
    , textFieldEvents
    -- * Armature
    , addArmatures
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

    , PageView
    , ListView
    , ScrollView
    , Slider
    , TextField
    , PageViewEvent(..)
    , ListViewEvent(..)
    , ScrollViewPos(..)
    , ScrollViewEvent(..)
    , SliderEvent(..)
    , TextFieldEvent(..)
    , loadCCS
    , loadCCS'
    ) where

import Diagrams (P2)
import Diagrams.BoundingBox
import Data.Tuple
import Data.Time.Clock
import qualified Data.Set as S
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Free
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Lens hiding (contains)
import Control.Monad.Ref
import Reflex
import Reflex.Extra
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Event
import JavaScript.Cocos2d.Schedule
import JavaScript.Cocos2d.CCS
import qualified JavaScript.Cocos2d.Async as A
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Attributes
import Linear.Affine

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
    touches :: Getter c (TouchEvents t)
    touchesBegan :: Getter c (Event t [Touch])
    touchesBegan = touches . to _touchesBegan
    touchesMoved :: Getter c (Event t [Touch])
    touchesMoved = touches . to _touchesMoved
    touchesEnded :: Getter c (Event t [Touch])
    touchesEnded = touches . to _touchesEnded
    touchesCancelled :: Getter c (Event t [Touch])
    touchesCancelled = touches . to _touchesCancelled

class HasSingleTouchEvents c t | c -> t where
    singleTouches :: Getter c (SingleTouchEvents t)
    touchBegan :: Getter c (Event t Touch)
    touchBegan = singleTouches . to _touchBegan
    touchMoved :: Getter c (Event t Touch)
    touchMoved = singleTouches . to _touchMoved
    touchEnded :: Getter c (Event t Touch)
    touchEnded = singleTouches . to _touchEnded
    touchCancelled :: Getter c (Event t Touch)
    touchCancelled = singleTouches . to _touchCancelled

filterSingular :: Reflex t => Event t [a] -> Event t a
filterSingular = fmapMaybe $ \case
                  [a] -> Just a
                  _ -> Nothing

instance HasTouchEvents (TouchEvents t) t where
    touches = id

touchesToTouchBegan, touchesToTouchMoved , touchesToTouchEnded, touchesToTouchCancelled
  :: (Reflex t, HasTouchEvents c t) => Getter c (Event t Touch)
touchesToTouchBegan = touchesBegan . to filterSingular
touchesToTouchMoved = touchesMoved . to filterSingular
touchesToTouchEnded = touchesEnded . to filterSingular
touchesToTouchCancelled = touchesCancelled . to filterSingular

touchesToSingleTouches
  :: HasSingleTouchEvents c t => Getter c (SingleTouchEvents t)
touchesToSingleTouches = to $ \c -> SingleTouchEvents { _touchBegan = c ^. touchBegan
                                                      , _touchMoved = c ^. touchMoved
                                                      , _touchEnded = c ^. touchEnded
                                                      , _touchCancelled = c ^. touchCancelled
                                                      }

instance Reflex t => HasSingleTouchEvents (TouchEvents t) t where
    touchBegan = touchesToTouchBegan
    touchMoved = touchesToTouchMoved
    touchEnded = touchesToTouchEnded
    touchCancelled = touchesToTouchCancelled
    singleTouches = touchesToSingleTouches

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
    touches = to $ TouchEvents <$> (select ?? TouchesBegan)
                               <*> (select ?? TouchesMoved)
                               <*> (select ?? TouchesEnded)
                               <*> (select ?? TouchesCancelled)
    touchesBegan = to (select ?? TouchesBegan)
    touchesEnded = to (select ?? TouchesEnded)
    touchesMoved = to (select ?? TouchesMoved)
    touchesCancelled = to (select ?? TouchesCancelled)

instance Reflex t => HasSingleTouchEvents (EventSelector t UIEventType) t where
    touchBegan = touchesToTouchBegan
    touchMoved = touchesToTouchMoved
    touchEnded = touchesToTouchEnded
    touchCancelled = touchesToTouchCancelled
    singleTouches = touchesToSingleTouches

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
          TouchesBegan -> wrap createTouchAllAtOnceEventListener $ setOnTouchesBegan ?? \t -> runWithActions ([et :=> Identity t], return ())
          TouchesEnded -> wrap createTouchAllAtOnceEventListener $ setOnTouchesEnded ?? \t -> runWithActions ([et :=> Identity t], return ())
          TouchesMoved -> wrap createTouchAllAtOnceEventListener $ setOnTouchesMoved ?? \t -> runWithActions ([et :=> Identity t], return ())
          TouchesCancelled -> wrap createTouchAllAtOnceEventListener $ setOnTouchesCancelled ?? \t -> runWithActions ([et :=> Identity t], return ())
          MouseDown -> wrap createMouseEventListener $ setOnMouseDown ?? \m -> runWithActions ([et :=> Identity m], return ())
          MouseUp -> wrap createMouseEventListener $ setOnMouseUp ?? \m -> runWithActions ([et :=> Identity m], return ())
          MouseMove -> wrap createMouseEventListener $ setOnMouseMove ?? \m -> runWithActions ([et :=> Identity m], return ())
          MouseScroll -> wrap createMouseEventListener $ setOnMouseScroll ?? \m -> runWithActions ([et :=> Identity m], return ())
          KeyPressed -> wrap createKeyboardEventListener $ setOnKeyPressed ?? \k -> runWithActions ([et :=> Identity k], return ())
          KeyReleased -> wrap createKeyboardEventListener $ setOnKeyReleased ?? \k -> runWithActions ([et :=> Identity k], return ())
          AccelChanged -> do
            (l, releaseCb) <- createAccelEventListener (\acc -> runWithActions ([et :=> Identity acc], return ()))
            addListener' l (-1)
            return $ removeListener l >> releaseCb
    return $! e

-- | Convenience function to obtain currently held keys
dynKeysDown :: (Reflex t, MonadHold t m, MonadFix m) => Event t Key -> Event t Key -> m (Dynamic t (S.Set Key))
dynKeysDown keyPressed keyReleased = accumMaybe (&) S.empty $ leftmost [ ffor keyPressed $ \k m -> do
                                                                            -- since KeyPressed event can keep firing
                                                                            -- we block non-changing events
                                                                            guard . not $ k `S.member` m
                                                                            return $ S.insert k m
                                                                       , ffor keyReleased $ fmap Just . S.delete
                                                                       ]

nodeContains :: (MonadIO m, IsNode n) => n -> (P2 Double) -> m Bool
nodeContains n p = do
    sz <- get n size
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
    rec e' <- onEvent began $ \t -> do
          (ms, _) <- sample b'
          (movedSeg, ms') <- breakE fst ms
          (endedSeg, ms'') <- headTailE ms'
          return (ms'', Just $ DragEvent t (snd <$> movedSeg) (snd <$> endedSeg))
        b' <- hold (mstream, Nothing) e'
    return $ fmapMaybe snd e'

-- | Modulate a ticker
dilate :: (Reflex t, MonadHold t m, MonadFix m, Num a, Ord a) => a -> Event t a -> m (Event t a)
dilate limit = mapAccumMaybe_ f (0, limit)
    where f (acc, l) d = let sum = acc + d in
                         if sum > l then (Just (0  , limit-(sum-l)) , Just sum)
                                    else (Just (sum, l            ) , Nothing )

-- | Get the tick per frame
ticks :: NodeGraph t m => m (Event t NominalDiffTime)
ticks = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
        scheduleUpdate 0 $ \d -> runWithActions ([et :=> Identity d], return ())

-- ticks' :: NodeGraph t m => NominalDiffTime -> m (Event t NominalDiffTime)
-- ticks' interval = do
--     runWithActions <- askRunWithActions
--     newEventWithTrigger $ \et ->
--         scheduleUpdate' interval True $ \d -> runWithActions [et :=> Identity d]

runRandEvent :: NodeGraph t m => Event t (Rand StdGen a) -> m (Event t a)
runRandEvent rands = do
    g <- liftIO newStdGen
    mapAccum_ (\g comp -> swap $ runRand comp g) g rands

-- | Delay an Event by the given amount of time
delay :: NodeGraph t m => NominalDiffTime -> Event t a -> m (Event t a)
delay dt e = do
    runWithActions <- askRunWithActions
    (e', trigger) <- newEventWithTriggerRef
    onEvent_ e $ \a -> mdo
      -- ignoring the tear-down function for now
      release <- scheduleUpdate' dt False $ \_ -> do
        readRef trigger >>= mapM_ (\t -> runWithActions ([t :=> Identity a], return ()))
        -- after finished with the update, release directly
        release
      return ()
    return e'

-- | Merge a deeply nested Event into a single Event
switchF :: NodeGraph t m => Free (Event t) a -> m (Event t a)
switchF f = switchF' f >>= \case
    Pure a -> fmap (a <$) askPostBuildEvent
    Free e -> return e

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
            runWithActions ([et :=> Identity (loaded+1, total)], return ())
    finished <- newEventWithTrigger $ \et ->
        A.setLoadFinish o $ runWithActions ([et :=> Identity ()], return ())
    askPostBuildEvent >>= runEvent_ . (A.load resources o <$)
    return (trigger, finished)


-------- WIDGET ----------

data WidgetTouchEvents t = WidgetTouchEvents { _widgetTouchBegan :: Event t (P2 Double)
                                             , _widgetTouchMoved :: Event t (P2 Double)
                                             , _widgetTouchEnded :: Event t (P2 Double)
                                             , _widgetTouchCancelled :: Event t ()
                                             }
makeClassyFor "HasWidgetTouchEvents" "widgetTouches" [ ("_widgetTouchBegan", "widgetTouchBegan")
                                                     , ("_widgetTouchMoved", "widgetTouchMoved")
                                                     , ("_widgetTouchEnded", "widgetTouchEnded")
                                                     , ("_widgetTouchCancelled", "widgetTouchCancelled")
                                                     ] ''WidgetTouchEvents

data WidgetEvents t = WidgetEvents { _weToWTouchEvents :: WidgetTouchEvents t
                                   , _widgetClicked :: Event t ()
                                   }
makeLenses ''WidgetEvents

instance HasWidgetTouchEvents (WidgetEvents t) t where
    widgetTouches = weToWTouchEvents


widgetEvents :: (NodeGraph t m, IsWidget w) => w -> m (WidgetEvents t)
widgetEvents w = do
    runWithActions <- askRunWithActions
    evt <- newEventWithTrigger $ \et ->
            setOnWidgetTouch w $ \t -> runWithActions ([et :=> Identity t], return ())
    let beganE =  fforMaybe evt $ \case
                    WidgetTouchBegan p -> Just p
                    _ -> Nothing
        movedE =  fforMaybe evt $ \case
                    WidgetTouchMoved p -> Just p
                    _ -> Nothing
        endedE =  fforMaybe evt $ \case
                    WidgetTouchEnded p -> Just p
                    _ -> Nothing
        cancelledE = fforMaybe evt $ \case
                      WidgetTouchCancelled -> Just ()
                      _ -> Nothing
    clicks <- newEventWithTrigger $ \et ->
      setOnWidgetClick w $ runWithActions ([et :=> Identity ()], return ())
    return $ WidgetEvents (WidgetTouchEvents beganE movedE endedE cancelledE) clicks

pageViewEvents :: NodeGraph t m => PageView -> m (Event t PageViewEvent)
pageViewEvents w = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
      setOnPageViewEvent w $ \e -> runWithActions ([et :=> Identity e], return ())

listViewEvents :: NodeGraph t m => ListView -> m (Event t ListViewEvent)
listViewEvents w = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
      setOnListViewEvent w $ \e -> runWithActions ([et :=> Identity e], return ())

scrollViewEvents :: NodeGraph t m => ScrollView -> m (Event t ScrollViewEvent)
scrollViewEvents w = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
      setOnScrollViewEvent w $ \e -> runWithActions ([et :=> Identity e], return ())

sliderEvents :: NodeGraph t m => Slider -> m (Event t SliderEvent)
sliderEvents w = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
      setOnSliderEvent w $ \e -> runWithActions ([et :=> Identity e], return ())

textFieldEvents :: NodeGraph t m => TextField -> m (Event t TextFieldEvent)
textFieldEvents w = do
    runWithActions <- askRunWithActions
    newEventWithTrigger $ \et ->
      setOnTextFieldEvent w $ \e -> runWithActions ([et :=> Identity e], return ())


-------- Armature ----------

addArmatures :: NodeGraph t m => [String]
                              -> m (Event t Double, Event t ()) -- ^ (Event of percentage progress, Event when everything finishes)
addArmatures files = do
    l <- createArmatureFileInfoAsyncListener
    addArmatureFileInfosAsync files l
    runWithActions <- askRunWithActions
    percentE <- newEventWithTrigger $ \et ->
      setOnArmatureFileInfoAsyncProgress l $ \percent -> runWithActions ([et :=> Identity percent], return ())
    -- count the number of percentE, all loading is finished when exactly the number of files is fired
    finished <- dropWhileE (< 1) percentE
    headFinished <- headE finished
    return (percentE, () <$ headFinished)
