{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Cocos2d.Widget
    ( getWidgetTouchEvents
    , getWidgetClicks
    , getWidgetEvents
    , findButtonByName
    , findTextByName
    , findLayoutByName
    -- data types
    , WidgetTouchEvents(WidgetTouchEvents)
    , HasWidgetTouchEvents(..)
    , WidgetEvents(WidgetEvents)
    , widgetClicked
    -- creation
    , button
    -- attrs
    , sizeType
    , positionType
    , sizePercent
    , positionPercent
    , titleText
    , titleColor
    , titleFontSize
    -- re-export --
    , PositionType(..)
    , SizeType(..)
    , Widget
    , WidgetPtr
    , Button
    , ButtonPtr
    , Text
    , TextPtr
    , Layout
    , LayoutPtr
    )
  where

import Data.Colour
import Data.Dependent.Sum ((==>))
import Diagrams (V2(..))
import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding (contains)

import Foreign.Hoppy.Runtime (Decodable(..), CppPtr(..))

import Graphics.UI.Cocos2d.Widget
import Graphics.UI.Cocos2d.Common

import Reflex
import Reflex.Host.Class
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes
import Reflex.Cocos2d.Types


data WidgetTouchEvents t = WidgetTouchEvents
    { _widgetTouchBegan     :: Event t ()
    , _widgetTouchMoved     :: Event t ()
    , _widgetTouchEnded     :: Event t ()
    , _widgetTouchCancelled :: Event t ()
    }

class HasWidgetTouchEvents c
                           t | c -> t where
  widgetTouchEvents :: Lens' c (WidgetTouchEvents t)
  widgetTouchBegan :: Lens' c (Event t ())
  {-# INLINE widgetTouchBegan #-}
  widgetTouchCancelled :: Lens' c (Event t ())
  {-# INLINE widgetTouchCancelled #-}
  widgetTouchEnded :: Lens' c (Event t ())
  {-# INLINE widgetTouchEnded #-}
  widgetTouchMoved :: Lens' c (Event t ())
  {-# INLINE widgetTouchMoved #-}
  widgetTouchBegan = (.) widgetTouchEvents widgetTouchBegan
  widgetTouchCancelled = (.) widgetTouchEvents widgetTouchCancelled
  widgetTouchEnded = (.) widgetTouchEvents widgetTouchEnded
  widgetTouchMoved = (.) widgetTouchEvents widgetTouchMoved
instance HasWidgetTouchEvents (WidgetTouchEvents t) t where
  {-# INLINE widgetTouchBegan #-}
  {-# INLINE widgetTouchCancelled #-}
  {-# INLINE widgetTouchEnded #-}
  {-# INLINE widgetTouchMoved #-}
  widgetTouchEvents = id
  widgetTouchBegan
    f
    (WidgetTouchEvents began moved ended cancelled)
    = fmap
        (\ began'
           -> WidgetTouchEvents began' moved ended cancelled)
        (f began)
  widgetTouchCancelled
    f
    (WidgetTouchEvents began moved ended cancelled)
    = fmap
        (\ cancelled'
           -> WidgetTouchEvents began moved ended cancelled')
        (f cancelled)
  widgetTouchEnded
    f
    (WidgetTouchEvents began moved ended cancelled)
    = fmap
        (\ ended'
           -> WidgetTouchEvents began moved ended' cancelled)
        (f ended)
  widgetTouchMoved
    f
    (WidgetTouchEvents began moved ended cancelled)
    = fmap
        (\ moved'
           -> WidgetTouchEvents began moved' ended cancelled)
        (f moved)

data WidgetEvents t = WidgetEvents
    { _weToWTouchEvents :: WidgetTouchEvents t
    , _widgetClicked    :: Event t ()
    }

weToWTouchEvents ::
  forall t.
  Lens' (WidgetEvents t) (WidgetTouchEvents t)
weToWTouchEvents f (WidgetEvents te clicked)
  = fmap
      (\ te' -> WidgetEvents te' clicked) (f te)
{-# INLINE weToWTouchEvents #-}
widgetClicked ::
  forall t. Lens' (WidgetEvents t) (Event t ())
widgetClicked f (WidgetEvents te clicked)
  = fmap
      (\ clicked' -> WidgetEvents te clicked') (f clicked)
{-# INLINE widgetClicked #-}

instance HasWidgetTouchEvents (WidgetEvents t) t where
    widgetTouchEvents = weToWTouchEvents

getWidgetTouchEvents ::
  (NodeBuilder t m, WidgetPtr w)
  => w -> m (WidgetTouchEvents t)
getWidgetTouchEvents w = do
    run <- view runWithActions
    touchTypes <- newEventWithTrigger $ \et -> do
      widget_addTouchEventListener w $ \_ t -> run ([et ==> t], return ())
      return $ pure ()
    let beganE = fforMaybe touchTypes $ \case
                  TouchEventType_Began -> Just ()
                  _ -> Nothing
        movedE = fforMaybe touchTypes $ \case
                   TouchEventType_Moved -> Just ()
                   _ -> Nothing
        endedE = fforMaybe touchTypes $ \case
                   TouchEventType_Ended -> Just ()
                   _ -> Nothing
        cancelledE = fforMaybe touchTypes $ \case
                       TouchEventType_Canceled -> Just ()
                       _ -> Nothing
    return $ WidgetTouchEvents beganE movedE endedE cancelledE

getWidgetClicks ::
  (NodeBuilder t m, WidgetPtr w)
  => w -> m (Event t ())
getWidgetClicks w = do
    run <- view runWithActions
    newEventWithTrigger $ \et -> do
      widget_addClickEventListener w $ \_ -> run ([et ==> ()], return ())
      return $ pure ()

getWidgetEvents ::
  (NodeBuilder t m, WidgetPtr w)
  => w -> m (WidgetEvents t)
getWidgetEvents w = WidgetEvents <$> getWidgetTouchEvents w <*> getWidgetClicks w

instance MonadIO m => HasRWTextAttrib Text m where
  text = hoistA liftIO $ Attrib (decode <=< text_getString) text_setString
  horizontalAlign = hoistA liftIO $ Attrib text_getTextHorizontalAlignment text_setTextHorizontalAlignment
  verticalAlign = hoistA liftIO $ Attrib text_getTextVerticalAlignment text_setTextVerticalAlignment
  textColor = hoistA liftIO $ Attrib (decode <=< text_getTextColor) text_setTextColor
  outline = WOAttrib set
    where set l (Just (Outline sColor sSize)) = liftIO $ text_enableOutlineWithSize l sColor sSize
          set l _ = liftIO $ text_disableLabelEffect l LabelEffect_Outline
  shadow = WOAttrib set
    where set l (Just (Shadow shColor shOffset shBlur)) = liftIO $ text_enableShadowWithOffset l shColor shOffset shBlur
          set l _ = liftIO $ text_disableLabelEffect l LabelEffect_Shadow
  glow = WOAttrib set
    where set l (Just (Glow glColor)) = liftIO $ text_enableGlow l glColor
          set l _ = liftIO $ text_disableLabelEffect l LabelEffect_Glow

-- helpers for finding widget
findWidgetByName' :: (MonadIO m, WidgetPtr w) => (Widget -> a) -> w -> String -> m (Maybe a)
findWidgetByName' downcast w name = liftIO $ do
    w' <- uiHelper_seekWidgetByName w name
    if w' == nullptr
      then return Nothing
      else return $ Just $ downcast w'
{-# INLINE findWidgetByName' #-}

findButtonByName :: (MonadIO m, WidgetPtr w) => w -> String -> m (Maybe Button)
findButtonByName = findWidgetByName' downToButton

findTextByName :: (MonadIO m, WidgetPtr w) => w -> String -> m (Maybe Text)
findTextByName = findWidgetByName' downToText

findLayoutByName :: (MonadIO m, WidgetPtr w) => w -> String -> m (Maybe Layout)
findLayoutByName = findWidgetByName' downToLayout

-- creating new widgets
button :: NodeBuilder t m => [Prop Button m] -> m (Button, Event t ())
button props = do
    but <- addNewChild button_create props
    we <- getWidgetClicks but
    return (but, we)

positionType :: (MonadIO m, WidgetPtr w) => Attrib' w m PositionType
positionType = hoistA liftIO $ Attrib widget_getPositionType widget_setPositionType

sizeType ::  (MonadIO m, WidgetPtr w) => Attrib' w m SizeType
sizeType = hoistA liftIO $  Attrib widget_getSizeType widget_setSizeType

sizePercent :: (MonadIO m, WidgetPtr w) => Attrib' w m (V2 Float)
sizePercent = hoistA liftIO $ Attrib (decode <=< widget_getSizePercent) widget_setSizePercent

positionPercent :: (MonadIO m, WidgetPtr w) => Attrib' w m (V2 Float)
positionPercent = hoistA liftIO $ Attrib (decode <=< widget_getPositionPercent) widget_setPositionPercent

titleText :: (MonadIO m, ButtonPtr b) => Attrib' b m String
titleText = hoistA liftIO $ Attrib button_getTitleText button_setTitleText

titleColor :: (MonadIO m, ButtonPtr b) => Attrib' b m (Colour Float)
titleColor = hoistA liftIO $ Attrib button_getTitleColor button_setTitleColor

titleFontSize :: (MonadIO m, ButtonPtr b) => Attrib' b m Float
titleFontSize = hoistA liftIO $ Attrib button_getTitleFontSize button_setTitleFontSize
