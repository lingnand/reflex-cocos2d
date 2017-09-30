{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Event
  ( MouseEvents(MouseEvents)
  , mouseDown
  , mouseUp
  , mouseMoved
  , mouseScrolled

  , TouchEvents(TouchEvents)
  , touchBegan
  , touchMoved
  , touchEnded
  , touchCancelled
  , MultiTouchEvents(MultiTouchEvents)
  , multiTouchBegan
  , multiTouchMoved
  , multiTouchEnded
  , multiTouchCancelled

  , KeyboardEvents(KeyboardEvents)
  , keyPressed
  , keyReleased

  , getMouseEvents
  , getTouchEvents
  , getMultiTouchEvents
  , getKeyboardEvents
  , getAccelerations

  , accumKeysDown

  , TouchSeq
  , touchFirst
  , touchLast
  , touchAll
  , accumTouchSeqEvent

  -- * Async
  , loadTexture
  -- , load
  -- * Utility
  -- , nodeContains
  -- * Widget
  -- , WidgetTouchEvents(WidgetTouchEvents)
  -- , HasWidgetTouchEvents(..)
  -- , WidgetEvents(WidgetEvents)
  -- , widgetClicked
  -- , widgetEvents
  -- , pageViewEvents
  -- , listViewEvents
  -- , scrollViewEvents
  -- , sliderEvents
  -- , textFieldEvents
  -- * Armature
  -- , addArmatures
  -- * re-export the lower level
  , KeyCode(..)

  -- , PageView
  -- , ListView
  -- , ScrollView
  -- , Slider
  -- , TextField
  -- , PageViewEvent(..)
  -- , ListViewEvent(..)
  -- , ScrollViewPos(..)
  -- , ScrollViewEvent(..)
  -- , SliderEvent(..)
  -- , TextFieldEvent(..)
  -- , loadCCS
  -- , loadCCS'
  ) where

-- import Diagrams.BoundingBox
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Lens hiding (contains)

import Graphics.UI.Cocos2d (Decodable(..), HasContents(..))

import Reflex

import qualified Foreign as F
import Graphics.UI.Cocos2d.Event hiding (Event)
import qualified Graphics.UI.Cocos2d.Event as CE
import Graphics.UI.Cocos2d.Director
import Graphics.UI.Cocos2d.Texture

import Reflex.Cocos2d.FastTriggerEvent.Class
import Reflex.Cocos2d.Types
import Reflex.Cocos2d.Internal.Global (globalEventDispatcher, globalTextureCache)

-- Event Packages
data MouseEvents t = MouseEvents
    { _mouseDown     :: Event t Mouse
    , _mouseUp       :: Event t Mouse
    , _mouseMoved    :: Event t Mouse
    , _mouseScrolled :: Event t Mouse
    }

mouseDown ::
  forall t. Lens' (MouseEvents t) (Event t Mouse)
mouseDown f (MouseEvents down up moved scrolled)
  = fmap
      (\ down' -> MouseEvents down' up moved scrolled)
      (f down)
{-# INLINE mouseDown #-}
mouseMoved ::
  forall t. Lens' (MouseEvents t) (Event t Mouse)
mouseMoved
  f
  (MouseEvents down up moved scrolled)
  = fmap
      (\ moved' -> MouseEvents down up moved' scrolled)
      (f moved)
{-# INLINE mouseMoved #-}
mouseScrolled ::
  forall t. Lens' (MouseEvents t) (Event t Mouse)
mouseScrolled
  f
  (MouseEvents down up moved scrolled)
  = fmap
      (\ scrolled' -> MouseEvents down up moved scrolled')
      (f scrolled)
{-# INLINE mouseScrolled #-}
mouseUp ::
  forall t. Lens' (MouseEvents t) (Event t Mouse)
mouseUp f (MouseEvents down up moved scrolled)
  = fmap
      (\ up' -> MouseEvents down up' moved scrolled)
      (f up)
{-# INLINE mouseUp #-}

data TouchEvents t = TouchEvents
    { _touchBegan     :: Event t Touch
    , _touchMoved     :: Event t Touch
    , _touchEnded     :: Event t Touch
    , _touchCancelled :: Event t Touch
    }

touchBegan ::
  forall t. Lens' (TouchEvents t) (Event t Touch)
touchBegan
  f
  (TouchEvents began moved ended cancelled)
  = fmap
      (\ began' -> TouchEvents began' moved ended cancelled)
      (f began)
{-# INLINE touchBegan #-}
touchCancelled ::
  forall t. Lens' (TouchEvents t) (Event t Touch)
touchCancelled
  f
  (TouchEvents began moved ended cancelled)
  = fmap
      (\ cancelled' -> TouchEvents began moved ended cancelled')
      (f cancelled)
{-# INLINE touchCancelled #-}
touchEnded ::
  forall t. Lens' (TouchEvents t) (Event t Touch)
touchEnded
  f
  (TouchEvents began moved ended cancelled)
  = fmap
      (\ ended' -> TouchEvents began moved ended' cancelled)
      (f ended)
{-# INLINE touchEnded #-}
touchMoved ::
  forall t. Lens' (TouchEvents t) (Event t Touch)
touchMoved
  f
  (TouchEvents began moved ended cancelled)
  = fmap
      (\ moved' -> TouchEvents began moved' ended cancelled)
      (f moved)
{-# INLINE touchMoved #-}

data MultiTouchEvents t = MultiTouchEvents
    { _multiTouchBegan     :: Event t [Touch]
    , _multiTouchMoved     :: Event t [Touch]
    , _multiTouchEnded     :: Event t [Touch]
    , _multiTouchCancelled :: Event t [Touch]
    }

multiTouchBegan ::
  forall t.
  Lens' (MultiTouchEvents t) (Event t [Touch])
multiTouchBegan
  f
  (MultiTouchEvents began moved ended cancelled)
  = fmap
      (\ began'
         -> MultiTouchEvents began' moved ended cancelled)
      (f began)
{-# INLINE multiTouchBegan #-}
multiTouchCancelled ::
  forall t.
  Lens' (MultiTouchEvents t) (Event t [Touch])
multiTouchCancelled
  f
  (MultiTouchEvents began moved ended cancelled)
  = fmap
      (\ cancelled'
         -> MultiTouchEvents began moved ended cancelled')
      (f cancelled)
{-# INLINE multiTouchCancelled #-}
multiTouchEnded ::
  forall t.
  Lens' (MultiTouchEvents t) (Event t [Touch])
multiTouchEnded
  f
  (MultiTouchEvents began moved ended cancelled)
  = fmap
      (\ ended'
         -> MultiTouchEvents began moved ended' cancelled)
      (f ended)
{-# INLINE multiTouchEnded #-}
multiTouchMoved ::
  forall t.
  Lens' (MultiTouchEvents t) (Event t [Touch])
multiTouchMoved
  f
  (MultiTouchEvents began moved ended cancelled)
  = fmap
      (\ moved'
         -> MultiTouchEvents began moved' ended cancelled)
      (f moved)
{-# INLINE multiTouchMoved #-}

data KeyboardEvents t = KeyboardEvents
    { _keyPressed  :: Event t KeyCode
    , _keyReleased :: Event t KeyCode
    }

keyPressed ::
  forall t.
  Lens' (KeyboardEvents t) (Event t KeyCode)
keyPressed f (KeyboardEvents pressed released)
  = fmap
      (\ pressed' -> KeyboardEvents pressed' released) (f pressed)
{-# INLINE keyPressed #-}
keyReleased ::
  forall t.
  Lens' (KeyboardEvents t) (Event t KeyCode)
keyReleased f (KeyboardEvents pressed released)
  = fmap
      (\ released' -> KeyboardEvents pressed released') (f released)
{-# INLINE keyReleased #-}

-- Event listeners
-- We generally put the listener to priority > scene graph (negative numbers)
getMouseEvents :: forall t m. (MonadIO m, FastTriggerEvent t m) => m (MouseEvents t)
getMouseEvents = do
    let wrapEvent :: (EventListenerMouse -> (EventMouse -> IO ()) -> IO ()) -> m (Event t Mouse)
        wrapEvent callbackSetter =
          fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
            l <- eventListenerMouse_create
            callbackSetter l $ \em -> do
              m <- decode em
              triggerFunc m (return ())
            eventDispatcher_addEventListenerWithFixedPriority globalEventDispatcher l (-1)
            return $ eventDispatcher_removeEventListener globalEventDispatcher l
    MouseEvents <$> wrapEvent eventListenerMouse_onMouseDown_set
                <*> wrapEvent eventListenerMouse_onMouseUp_set
                <*> wrapEvent eventListenerMouse_onMouseMove_set
                <*> wrapEvent eventListenerMouse_onMouseScroll_set

getTouchEvents :: forall t m. (MonadIO m, FastTriggerEvent t m) => m (TouchEvents t)
getTouchEvents = do
    -- XXX: we have to create a listener instance and keep it in memory as
    -- multiple events might depend on it (cannot clean up easily per individual
    -- event; cannot create one listener per event either as
    -- {move,end,cancel}touch events can only be handled if begin events are
    -- already handled by a listener)
    l <- liftIO eventListenerTouchOneByOne_create
    let wrapEvent
          :: ((EventTouch -> CE.Event -> IO ()) -> IO (F.FunPtr f))
          -> (EventListenerTouchOneByOne -> F.FunPtr f -> IO ())
          -> m (Event t Touch)
        wrapEvent newFunPtr setFunPtr =
          fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
            fp <- newFunPtr $  \et _ -> do
              t <- decode et
              triggerFunc t (return ())
            setFunPtr l fp
            eventDispatcher_addEventListenerWithFixedPriority globalEventDispatcher l (-1)
            return $ do
              setFunPtr l F.nullFunPtr -- reset to nullptr callback
              F.freeHaskellFunPtr fp -- release the fun ptr
        touchBegan_newFunPtr' cb =
          -- always return True (handles the Touch)
          eventTouchBeganCallback_newFunPtr $ \et evt -> cb et evt >> return True
    TouchEvents
      <$> wrapEvent touchBegan_newFunPtr' eventListenerTouchOneByOne_onTouchBegan_set
      <*> wrapEvent eventTouchCallback_newFunPtr eventListenerTouchOneByOne_onTouchMoved_set
      <*> wrapEvent eventTouchCallback_newFunPtr eventListenerTouchOneByOne_onTouchEnded_set
      <*> wrapEvent eventTouchCallback_newFunPtr eventListenerTouchOneByOne_onTouchCancelled_set

getMultiTouchEvents :: forall t m. (MonadIO m, FastTriggerEvent t m) => m (MultiTouchEvents t)
getMultiTouchEvents = do
    let wrapEvent :: (EventListenerTouchAllAtOnce -> (EventTouchVectorConst -> CE.Event -> IO ()) -> IO ())
                  -> m (Event t [Touch])
        wrapEvent callbackSetter =
          fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
            l <- eventListenerTouchAllAtOnce_create
            callbackSetter l $ \ets _ -> do
              -- convert ets to a list of Touches
              ts <- toContents ets >>= mapM decode
              triggerFunc ts (return ())
            eventDispatcher_addEventListenerWithFixedPriority globalEventDispatcher l (-1)
            return $ eventDispatcher_removeEventListener globalEventDispatcher l
    MultiTouchEvents <$> wrapEvent eventListenerTouchAllAtOnce_onTouchesBegan_set
                     <*> wrapEvent eventListenerTouchAllAtOnce_onTouchesMoved_set
                     <*> wrapEvent eventListenerTouchAllAtOnce_onTouchesEnded_set
                     <*> wrapEvent eventListenerTouchAllAtOnce_onTouchesCancelled_set

getKeyboardEvents :: forall t m. (MonadIO m, FastTriggerEvent t m) => m (KeyboardEvents t)
getKeyboardEvents = do
    let wrapEvent :: (EventListenerKeyboard -> (KeyCode -> CE.Event -> IO ()) -> IO ())
                  -> m (Event t KeyCode)
        wrapEvent callbackSetter =
          fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
            l <- eventListenerKeyboard_create
            callbackSetter l $ \kc _ -> triggerFunc kc (return ())
            eventDispatcher_addEventListenerWithFixedPriority globalEventDispatcher l (-1)
            return $ eventDispatcher_removeEventListener globalEventDispatcher l
    KeyboardEvents <$> wrapEvent eventListenerKeyboard_onKeyPressed_set
                   <*> wrapEvent eventListenerKeyboard_onKeyReleased_set

getAccelerations :: FastTriggerEvent t m => m (Event t Acceleration)
getAccelerations = fastNewEventWithLazyTriggerWithOnComplete $ \triggerFunc -> do
    l <- eventListenerAcceleration_create $ \ea _ -> do
            a <- decode ea
            triggerFunc a (return ())
    eventDispatcher_addEventListenerWithFixedPriority globalEventDispatcher l (-1)
    return $ eventDispatcher_removeEventListener globalEventDispatcher l

-- | Convenience function to obtain currently held keys
accumKeysDown ::
  (Reflex t, MonadHold t m, MonadFix m)
  => KeyboardEvents t -> m (Dynamic t (S.Set KeyCode))
accumKeysDown (KeyboardEvents pressedE releasedE) = do
    let insertE = ffor pressedE $ \k m -> do
                    -- since KeyPressed event can keep firing
                    -- we block non-changing events
                    guard . not $ k `S.member` m
                    return $ S.insert k m
        deleteE = ffor releasedE $ fmap Just . S.delete
    accumMaybe (&) S.empty $ leftmost [ insertE, deleteE ]

-- nodeContains :: (MonadIO m, IsNode n) => n -> (P2 Double) -> m Bool
-- nodeContains n p = do
--     sz <- get n size
--     p' <- convertToNodeSpace n p
--     return $ fromCorners 0 (0.+^sz) `contains` p'

data TouchSeq = TouchSeq
  { _touchFirst :: Touch
  -- ^ reverse ordered subsequent moves for easy 'appending'
  , _touchMoves :: [Touch]
  , _touchLast  :: Maybe Touch
  } deriving Eq

touchFirst :: Lens' TouchSeq Touch
touchFirst f (TouchSeq first nexts last) = (\first' -> TouchSeq first' nexts last) <$> f first
{-# INLINE touchFirst #-}

touchLast :: Traversal' TouchSeq Touch
touchLast _ seq@(TouchSeq _ _ Nothing) = pure seq
touchLast f (TouchSeq first moves (Just last)) =
  (\last' -> TouchSeq first moves (Just last')) <$> f last
{-# INLINE touchLast #-}

touchAll :: Getter TouchSeq [Touch]
touchAll = to $ \(TouchSeq first nexts last) -> (first:reverse nexts) ++ maybeToList last
{-# INLINE touchAll #-}

accumTouchSeqEvent
  :: (Reflex t, MonadHold t m, MonadFix m)
  => TouchEvents t -> m (Event t TouchSeq)
accumTouchSeqEvent (TouchEvents began moved ended cancelled) = do
  accumed <- accum (&) Nothing $ mergeWith (.)
    [ updateBegin <$> began
    , updateMove <$> moved
    , updateEnd <$> ended
    , updateEnd <$> cancelled
    ]
  return $ fmapMaybe id (updated accumed)
  where
    updateBegin t _ = Just $ TouchSeq t [] Nothing
    -- if the previous seq is finished, we need to start a new one
    updateMove t (Just (TouchSeq _ _ (Just _))) = Just $ TouchSeq t [] Nothing
    updateMove t (Just seq) = Just $ seq{ _touchMoves = t:_touchMoves seq }
    updateMove _ _ = Nothing
    updateEnd t (Just seq) = Just $ seq{ _touchLast = Just t }
    updateEnd _ _ = Nothing

-- | Delay an Event by the given amount of seconds
-- TODO: every time this is called the previous "delayed" is invalidated
-- we need to have a unique key for every invocation
-- delay :: NodeGraph t m => Time -> Event t a -> m (Event t a)
-- delay dt e = do
--     runWithActions <- askRunWithActions
--     (e', trigger) <- newEventWithTriggerRef
--     delayedFire <- liftIO $ do
--       dtor <- director_getInstance
--       sch <- director_getScheduler dtor
--       let target = castPtr $ toPtr dtor
--       return $ \a ->
--         scheduler_scheduleWithIntervalAndRepeat sch
--           (\_ -> readRef trigger >>= mapM_ (\t -> runWithActions ([t ==> a], return ())))
--           target 0 0 dt False "delayed"
--     onEvent_ e $ liftIO . delayedFire
--     return e'

-- | NOTE: we can't return the texture because it's an autoreleased object
-- Also - we run the actual loading on postBuild so we rule out possibility
-- of incomplete network when loading finishes
loadTexture :: ( MonadIO m, FastTriggerEvent t m, PostBuild t m
               , PerformEvent t m, MonadIO (Performable m) )
            => String -> m (Event t ())
loadTexture path = do
    -- Since we are not sure if the user would subscribe to the resulting event, we can't just use
    -- lazyTrigger
    (e, triggerFunc) <- fastNewTriggerEvent
    postE <- getPostBuild
    performEvent_ . ffor postE $ \_ -> liftIO $
      textureCache_addImageAsync globalTextureCache path $ \_ -> triggerFunc ()
    return e

-- | Load a list of resources in an async manner
-- returns (Event t (loaded, total), finished)
-- NOTE: we reverse the @loaded@ and @total@ because this makes more sense
-- also, increment the finished by 1 because we are not procedurally
-- looking at how many loaded /last time/ (instead how many /already/
-- loaded)
-- load :: NodeGraph t m => [String] -> m (Event t (Int, Int), Event t ())
-- load resources = do
--     o <- A.createLoadOption
--     runWithActions <- askRunWithActions
--     trigger <- newEventWithTrigger $ \et ->
--         A.setLoadTrigger o $ \total loaded ->
--             runWithActions ([et :=> Identity (loaded+1, total)], return ())
--     finished <- newEventWithTrigger $ \et ->
--         A.setLoadFinish o $ runWithActions ([et :=> Identity ()], return ())
--     askPostBuildEvent >>= runEvent_ . (A.load resources o <$)
--     return (trigger, finished)


-------- Armature ----------

-- addArmatures :: NodeGraph t m => [String]
--                               -> m (Event t Double, Event t ()) -- ^ (Event of percentage progress, Event when everything finishes)
-- addArmatures files = do
--     l <- createArmatureFileInfoAsyncListener
--     addArmatureFileInfosAsync files l
--     runWithActions <- askRunWithActions
--     percentE <- newEventWithTrigger $ \et ->
--       setOnArmatureFileInfoAsyncProgress l $ \percent -> runWithActions ([et :=> Identity percent], return ())
--     -- count the number of percentE, all loading is finished when exactly the number of files is fired
--     finished <- dropWhileE (< 1) percentE
--     headFinished <- headE finished
--     return (percentE, () <$ headFinished)
