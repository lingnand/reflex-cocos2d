{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Cocos2d.Label
    ( label
    , label_
    -- attrs --
    , lineBreakWithoutSpace
    , maxLineWidth
    , boundingSize
    , boundingWidth
    , boundingHeight
    , clipMargin

    , systemFontName
    , systemFontSize
    -- re-export --
    , Label
    , LabelPtr
    , TextHAlignment(..)
    , TextVAlignment(..)
    )
  where

import Diagrams (V2(..))
import Control.Monad
import Control.Monad.Trans

import Foreign.Hoppy.Runtime (Decodable(..))
import Graphics.UI.Cocos2d.Common
import Graphics.UI.Cocos2d.Label

import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Types

label :: NodeBuilder t m => [Prop Label m] -> m Label
label = addNewChild label_create

label_ :: NodeBuilder t m => [Prop Label m] -> m ()
label_ = void . label

---- Attrs ----
-- General Attributes
instance MonadIO m => HasRWTextAttrib Label m where
  text = hoistA liftIO $ Attrib (decode <=< label_getString) label_setString
  horizontalAlign = hoistA liftIO $ Attrib label_getHorizontalAlignment label_setHorizontalAlignment
  verticalAlign = hoistA liftIO $ Attrib label_getVerticalAlignment label_setVerticalAlignment
  textColor = hoistA liftIO $ Attrib (decode <=< label_getTextColor) label_setTextColor
  outline = WOAttrib set
    where set l (Just (Outline sColor sSize)) = liftIO $ label_enableOutlineWithSize l sColor sSize
          set l _ = liftIO $ label_disableLabelEffect l LabelEffect_Outline
  shadow = WOAttrib set
    where set l (Just (Shadow shColor shOffset shBlur)) = liftIO $ label_enableShadowWithOffset l shColor shOffset shBlur
          set l _ = liftIO $ label_disableLabelEffect l LabelEffect_Shadow
  glow = WOAttrib set
    where set l (Just (Glow glColor)) = liftIO $ label_enableGlow l glColor
          set l _ = liftIO $ label_disableLabelEffect l LabelEffect_Glow

lineBreakWithoutSpace :: (MonadIO m, LabelPtr l) => WOAttrib' l m Bool
lineBreakWithoutSpace = WOAttrib $ \l -> liftIO . label_setLineBreakWithoutSpace l

maxLineWidth :: (MonadIO m, LabelPtr l) => Attrib' l m Float
maxLineWidth = hoistA liftIO $ Attrib label_getMaxLineWidth label_setMaxLineWidth

-- | corresponding to setDimensions
boundingSize :: (MonadIO m, LabelPtr l) => Attrib' l m (V2 Float)
boundingSize = hoistA liftIO $ Attrib (decode <=< label_getDimensions) (\l (V2 w h) -> label_setDimensions l w h)

-- | corresponding to setWidth
boundingWidth :: (MonadIO m, LabelPtr l) => Attrib' l m Float
boundingWidth = hoistA liftIO $ Attrib label_getWidth label_setWidth

-- | corresponding to setHeight
boundingHeight :: (MonadIO m, LabelPtr l) => Attrib' l m Float
boundingHeight = hoistA liftIO $ Attrib label_getHeight label_setHeight

clipMargin :: (MonadIO m, LabelPtr l) => Attrib' l m Bool
clipMargin = hoistA liftIO $ Attrib label_isClipMarginEnabled label_setClipMarginEnabled

-- System font
systemFontName :: (MonadIO m, LabelPtr l) => Attrib' l m String
systemFontName = hoistA liftIO $ Attrib (decode <=< label_getSystemFontName) label_setSystemFontName

systemFontSize :: (MonadIO m, LabelPtr l) => Attrib' l m Float
systemFontSize = hoistA liftIO $ Attrib label_getSystemFontSize label_setSystemFontSize

-- TODO
-- lineHeight
-- lineSpacing
-- additionalKerning

