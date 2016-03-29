{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Utils
    (
      appDyn
    , setV
    , BaseConfig(..)
    , HasBaseConfig(baseConfig, anchor, skew, zIndex, scale, visible, opacity, cascadeColor, cascadeOpacity)
    , ColorConfig(..)
    , HasColorConfig(..)
    , SizeConfig(..)
    , HasSizeConfig(..)
    , appBaseConfig
    , appColorConfig
    , appSizeConfig
    )
  where

import Data.Colour
import Data.Default
import Diagrams hiding (sample, opacity, _opacity, scale, size)
import Control.Lens hiding (contains)
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import Reflex.Trans
import Reflex.Cocos2d.Class

-- * Various helper functions
appDyn :: NodeGraph t m => (a -> HostFrame t ()) -> Dynamic t a -> m ()
appDyn setter dyn = do
    schedulePostBuild $ setter =<< sample (current dyn)
    forH_ (updated dyn) $ setter

setV :: Monad m => (n -> a -> m ()) -> (n -> a -> m ()) -> (n -> V2 a -> m ())
setV setX setY n (V2 x y) = setX n x >> setY n y

-- * Config components
data BaseConfig t = BaseConfig
    { _bToTrans :: Trans t
    , _anchor :: Dynamic t (V2 Double)
    , _skew :: Dynamic t (V2 Double)
    , _zIndex :: Dynamic t Int
    , _scale :: Dynamic t (V2 Double)
    , _visible :: Dynamic t Bool
    , _opacity :: Dynamic t Double -- ^ 0.0 - 1.0
    , _cascadeColor :: Bool
    , _cascadeOpacity :: Bool
    }
makeClassy ''BaseConfig

instance HasBaseConfig c t => HasTrans c t where
    trans = bToTrans

instance Reflex t => Default (BaseConfig t) where
    def = BaseConfig { _bToTrans = def
                     , _anchor = constDyn 0
                     , _skew = constDyn 0
                     , _zIndex = constDyn 0
                     , _scale = constDyn $ pure 1.0
                     , _visible = constDyn True
                     , _opacity = constDyn 1.0
                     , _cascadeColor = False
                     , _cascadeOpacity = False
                     }

newtype ColorConfig t = ColorConfig { _color :: Dynamic t (Colour Double) }
makeClassy ''ColorConfig

instance Reflex t => Default (ColorConfig t) where
    def = ColorConfig { _color = constDyn black }

newtype SizeConfig t = SizeConfig { _size :: Dynamic t (V2 Double) }
makeClassy ''SizeConfig


instance Reflex t => Default (SizeConfig t) where
    def = SizeConfig { _size = constDyn 0 }

appBaseConfig :: (IsNode n, NodeGraph t m, HasBaseConfig c t) => c -> n -> m ()
appBaseConfig c n = do
    let BaseConfig (Trans pos rotation) anchor skew zIndex scale visible opacity cascadeColor cascadeOpacity = c^.baseConfig
    appDyn (\p -> let (x, y) = unp2 p in setPosition n x y) pos
    appDyn (setV setAnchorX setAnchorY n) anchor
    appDyn (setV setSkewX setSkewY n) skew
    appDyn (setZIndex n) zIndex
    appDyn (setRotation n . negate . (^._theta.deg)) rotation
    appDyn (setV setScaleX setScaleY n) scale
    appDyn (setVisible n) visible
    appDyn (setOpacity n) opacity
    setCascadeColor n cascadeColor
    setCascadeOpacity n cascadeOpacity

appColorConfig :: (IsNode n, NodeGraph t m, HasColorConfig c t) => c -> n -> m ()
appColorConfig c n = appDyn (setColor n) (c^.color)

appSizeConfig :: (IsNode n, NodeGraph t m, HasSizeConfig c t) => c -> n -> m ()
appSizeConfig c n = appDyn (setV setWidth setHeight n) (c^.size)
