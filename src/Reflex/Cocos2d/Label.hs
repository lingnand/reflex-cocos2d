{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.Cocos2d.Label
    (
      Stroke(Stroke)
    , strokeColor
    , strokeSize
    , Shadow(Shadow)
    , shadowColor
    , shadowOffset
    , shadowBlur
    , label
    , label_
    -- attrs --
    , text
    , horizontalAlign
    , verticalAlign
    , fontName
    , fontSize
    , fontColor
    , boundingSize
    , boundingWidth
    , boundingHeight
    , stroke
    , shadow
    -- re-export --
    , Label
    , IsLabel(..)
    ) where

import Data.Colour
import Data.Colour.Names
import Data.Default
import Diagrams (V2(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens hiding (set)
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Label
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes

data Stroke = Stroke { _strokeColor :: Colour Double
                     , _strokeSize :: Double
                     } deriving (Show, Eq)
makeLenses ''Stroke

instance Default Stroke where
    def = Stroke { _strokeColor = white
                 , _strokeSize = 0
                 }

data Shadow = Shadow { _shadowColor :: AlphaColour Double
                     , _shadowOffset :: V2 Double
                     , _shadowBlur :: Double
                     } deriving (Show, Eq)
makeLenses ''Shadow

instance Default Shadow where
    def = Shadow { _shadowColor = white `withOpacity` 0.5
                 , _shadowOffset = 0
                 , _shadowBlur = 0
                 }

label :: NodeGraph t m => [Prop Label m] -> m Label
label props = do
    l <- createLabel
    set l props
    askParent >>= flip addChild l
    return l

label_ :: NodeGraph t m => [Prop Label m] -> m ()
label_ = void . label

---- attrs ----
text :: (MonadIO m, IsLabel l) => Attrib l m String
text = attrib getText setText

horizontalAlign :: (MonadIO m, IsLabel l) => Attrib l m HAlign
horizontalAlign = attrib getHorizontalAlign setHorizontalAlign

verticalAlign :: (MonadIO m, IsLabel l) => Attrib l m VAlign
verticalAlign = attrib getVerticalAlign setVerticalAlign

fontName :: (MonadIO m, IsLabel l) => Attrib l m String
fontName = attrib getFontName setFontName

fontSize :: (MonadIO m, IsLabel l) => Attrib l m Double
fontSize = attrib getFontSize setFontSize

fontColor :: (MonadIO m, IsLabel l) => Attrib l m (Colour Double)
fontColor = attrib getFontFillColor setFontFillColor

boundingSize :: (MonadIO m, IsLabel l) => Attrib l m (V2 Double)
boundingSize = attrib getBoundingSize setBoundingSize

boundingWidth :: (MonadIO m, IsLabel l) => Attrib l m Double
boundingWidth = attrib getBoundingWidth setBoundingWidth

boundingHeight :: (MonadIO m, IsLabel l) => Attrib l m Double
boundingHeight = attrib getBoundingHeight setBoundingHeight

-- | Just Stroke to turn on stroke, Nothing to turn it off
stroke :: (MonadIO m, IsLabel l) => SetOnlyAttrib l m (Maybe Stroke)
stroke = SetOnlyAttrib' set
  where set l (Just (Stroke sColor sSize)) = enableStroke l sColor sSize
        set l _ = disableStroke l

shadow :: (MonadIO m, IsLabel l) => SetOnlyAttrib l m (Maybe Shadow)
shadow = SetOnlyAttrib' set
  where set l (Just (Shadow shColor (V2 offsetX offsetY) shBlur)) = enableShadow l shColor offsetX offsetY shBlur
        set l _ = disableShadow l
