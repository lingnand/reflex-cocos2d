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
    , LabelConfig
    , HasLabelConfig(..)
    , DynLabel
    , label
    , label_
    ) where

import Data.Colour
import Data.Colour.Names
import Data.Default
import Diagrams hiding (size)
import Control.Monad
import Control.Lens
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Label
import Reflex
import Reflex.Trans
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Utils

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

data LabelConfig t = LabelConfig { _lbToNodeConfig :: NodeConfig t -- ^ SizeConfig operates on boundingbox
                                 , _lbToText :: Dynamic t String
                                 , _lbToHorizontalAlign :: Dynamic t HAlign
                                 , _lbToVerticalAlign :: Dynamic t VAlign
                                 , _lbToFontName :: Dynamic t String
                                 , _lbToFontSize :: Dynamic t Double
                                 , _lbToFontColor :: Dynamic t (Colour Double)
                                 , _lbToStroke :: Dynamic t (Maybe Stroke)
                                 , _lbToShadow :: Dynamic t (Maybe Shadow)
                                 }
makeLenses ''LabelConfig

class HasNodeConfig c t => HasLabelConfig c t | c -> t where
    labelConfig :: Lens' c (LabelConfig t)
    text :: Lens' c (Dynamic t String)
    text = labelConfig . lbToText
    horizontalAlign :: Lens' c (Dynamic t HAlign)
    horizontalAlign = labelConfig . lbToHorizontalAlign
    verticalAlign :: Lens' c (Dynamic t VAlign)
    verticalAlign = labelConfig . lbToVerticalAlign
    fontName :: Lens' c (Dynamic t String)
    fontName = labelConfig . lbToFontName
    fontSize :: Lens' c (Dynamic t Double)
    fontSize = labelConfig . lbToFontSize
    fontColor :: Lens' c (Dynamic t (Colour Double))
    fontColor = labelConfig . lbToFontColor
    stroke :: Lens' c (Dynamic t (Maybe Stroke))
    stroke = labelConfig . lbToStroke
    shadow :: Lens' c (Dynamic t (Maybe Shadow))
    shadow = labelConfig . lbToShadow

instance HasLabelConfig (LabelConfig t) t where
    labelConfig = id

instance HasNodeConfig (LabelConfig t) t where
    nodeConfig = lbToNodeConfig

instance HasBaseConfig (LabelConfig t) t where
    baseConfig = nodeConfig . baseConfig

instance HasTrans (LabelConfig t) t where
    trans = nodeConfig . trans

instance HasSizeConfig (LabelConfig t) t where
    sizeConfig = nodeConfig . sizeConfig

instance Reflex t => Default (LabelConfig t) where
    def = LabelConfig { _lbToNodeConfig = def & anchor .~ constDyn (pure 0.5)
                      , _lbToText = constDyn ""
                      , _lbToHorizontalAlign =  constDyn ALeft
                      , _lbToVerticalAlign = constDyn ATop
                      , _lbToFontName = constDyn "Arial"
                      , _lbToFontSize = constDyn 16
                      , _lbToFontColor = constDyn white
                      , _lbToStroke = constDyn Nothing
                      , _lbToShadow = constDyn Nothing
                      }

data DynLabel t = DynLabel (LabelConfig t) Label

instance HasLabelConfig (DynLabel t) t where
    labelConfig f (DynLabel c l) = (\c' -> DynLabel c' l) <$> f c

instance HasNodeConfig (DynLabel t) t where
    nodeConfig = labelConfig . nodeConfig

instance HasBaseConfig (DynLabel t) t where
    baseConfig = labelConfig . baseConfig

instance HasTrans (DynLabel t) t where
    trans = labelConfig . trans

instance HasSizeConfig (DynLabel t) t where
    sizeConfig = labelConfig . sizeConfig

instance IsNode (DynLabel t) where
    toNode (DynLabel _ l) = toNode l

instance IsLabel (DynLabel t) where
    toLabel (DynLabel _ l) = l

label :: NodeGraph t m => LabelConfig t -> m (DynLabel t)
label conf@(LabelConfig nc text hAlign vAlign fName fSize fColor stroke shadow) = do
    l <- createLabel
    appBaseConfig nc l
    appDyn (\(V2 x y) -> setDimensions l x y) (nc^.size)
    appDyn (setText l) text
    appDyn (setHorizontalAlign l) hAlign
    appDyn (setVerticalAlign l) vAlign
    appDyn (setFontName l) fName
    appDyn (setFontSize l) fSize
    appDyn (setFontFillColor l) fColor
    appDyn ?? stroke $ \case
        Just (Stroke sColor sSize) -> enableStroke l sColor sSize
        _ -> disableStroke l
    appDyn ?? shadow $ \case
        Just (Shadow shColor (V2 offsetX offsetY) shBlur) -> enableShadow l shColor offsetX offsetY shBlur
        _ -> disableShadow l
    askParent >>= flip addChild l
    return $ DynLabel conf l

label_ :: NodeGraph t m => LabelConfig t -> m ()
label_ = void . label
