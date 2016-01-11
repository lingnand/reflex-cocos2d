{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node
    (
      NodeConfig
    , LayerConfig
    , LayerColorConfig
    , HasBaseConfig(..)
    , HasSizeConfig(..)
    , HasColorConfig(..)
    , HasNodeConfig(..)
    , HasLayerConfig(..)
    , HasLayerColorConfig(..)
    , DynNode
    , DynLayer
    , DynLayerColor
    , node
    , node_
    , layer
    , layer_
    , layerColor
    , layerColor_
    ) where

import Data.Dependent.Sum (DSum (..))
import Data.Colour
import Data.Colour.Names
import Data.Default
import Control.Monad
import Control.Monad.Ref
import Control.Monad.IO.Class
import Control.Lens
import Linear
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import Reflex.Cocos2d.Class

data BaseConfig t = BaseConfig
    { _position :: Dynamic t (V2 Double)
    , _anchor :: Dynamic t (V2 Double)
    , _skew :: Dynamic t (V2 Double)
    , _zIndex :: Dynamic t Int
    , _rotation :: Dynamic t (V2 Double)
    , _scale :: Dynamic t (V2 Double)
    , _visible :: Dynamic t Bool
    , _opacity :: Dynamic t Double -- ^ 0.0 - 1.0
    , _cascadeColor :: Bool
    , _cascadeOpacity :: Bool
    }
makeClassy ''BaseConfig

newtype ColorConfig t = ColorConfig { _color :: Dynamic t (Colour Double) }
makeClassy ''ColorConfig

newtype SizeConfig t = SizeConfig { _size :: Dynamic t (V2 Double) }
makeClassy ''SizeConfig


data NodeConfig t = NodeConfig { _nToBaseConfig :: BaseConfig t
                               , _nToSizeConfig :: SizeConfig t
                               }
makeLenses ''NodeConfig

class (HasBaseConfig c t, HasSizeConfig c t) => HasNodeConfig c t | c -> t where
    nodeConfig :: Lens' c (NodeConfig t)

instance {-# OVERLAPPABLE #-} HasNodeConfig c t => HasBaseConfig c t where
    baseConfig = nodeConfig . nToBaseConfig

instance {-# OVERLAPPABLE #-} HasNodeConfig c t => HasSizeConfig c t where
    sizeConfig = nodeConfig . nToSizeConfig

instance HasNodeConfig (NodeConfig t) t where
    nodeConfig = id


newtype LayerConfig t = LayerConfig { _lToNodeConfig :: NodeConfig t }
makeLenses ''LayerConfig

class HasNodeConfig c t => HasLayerConfig c t | c -> t where
    layerConfig :: Lens' c (LayerConfig t)

instance {-# OVERLAPPABLE #-} HasLayerConfig c t => HasNodeConfig c t where
    nodeConfig = layerConfig . lToNodeConfig

instance HasLayerConfig (LayerConfig t) t where
    layerConfig = id


data LayerColorConfig t = LayerColorConfig { _lcToLayerConfig :: LayerConfig t
                                           , _lcToColorConfig :: ColorConfig t
                                           }
makeLenses ''LayerColorConfig

class (HasLayerConfig c t, HasColorConfig c t) => HasLayerColorConfig c t | c -> t where
    layerColorConfig :: Lens' c (LayerColorConfig t)

instance {-# OVERLAPPABLE #-} HasLayerColorConfig c t => HasLayerConfig c t where
    layerConfig = layerColorConfig . lcToLayerConfig

instance {-# OVERLAPPABLE #-} HasLayerColorConfig c t => HasColorConfig c t where
    colorConfig = layerColorConfig . lcToColorConfig

instance HasLayerColorConfig (LayerColorConfig t) t where
    layerColorConfig = id


instance Reflex t => Default (NodeConfig t) where
    def = NodeConfig bc sc
      where bc = BaseConfig { _position = constDyn zero
                            , _anchor = constDyn zero
                            , _skew = constDyn zero
                            , _zIndex = constDyn 0
                            , _rotation = constDyn zero
                            , _scale = constDyn $ pure 1.0
                            , _visible = constDyn True
                            , _opacity = constDyn 1.0
                            , _cascadeColor = False
                            , _cascadeOpacity = False
                            }
            sc = SizeConfig { _size = constDyn zero }

instance Reflex t => Default (LayerConfig t) where
    def = LayerConfig $ def & anchor .~ constDyn (pure 0.5)
                            & size .~ constDyn (V2 winWidth winHeight)

instance Reflex t => Default (LayerColorConfig t) where
    def = LayerColorConfig def $ ColorConfig { _color = constDyn black }

-- * Node

data DynNode t = DynNode (NodeConfig t) Node

instance HasNodeConfig (DynNode t) t where
    nodeConfig f (DynNode c n) = (\c' -> DynNode c' n) <$> f c

instance IsNode (DynNode t) where
    toNode (DynNode _ n) = n

node :: NodeGraph t m => NodeConfig t -> m (DynNode t)
node conf = do
    n <- createNode
    appNodeConfig conf n
    askParent >>= flip addChild n
    return $ DynNode conf n

node_ :: NodeGraph t m => NodeConfig t -> m ()
node_ = void . node

-- * Layer

data DynLayer t = DynLayer (LayerConfig t) Layer

instance HasLayerConfig (DynLayer t) t where
    layerConfig f (DynLayer c l) = (\c' -> DynLayer c' l) <$> f c

instance IsNode (DynLayer t) where
    toNode (DynLayer _ l) = toNode l

instance IsLayer (DynLayer t) where
    toLayer (DynLayer _ l) = l

layer :: NodeGraph t m => LayerConfig t -> m (DynLayer t)
layer conf = do
    l <- createLayer
    appNodeConfig conf l
    askParent >>= flip addChild l
    return $ DynLayer conf l

layer_ :: NodeGraph t m => LayerConfig t -> m ()
layer_ = void . layer

-- * LayerColor

data DynLayerColor t = DynLayerColor (LayerColorConfig t) LayerColor

instance HasLayerColorConfig (DynLayerColor t) t where
    layerColorConfig f (DynLayerColor c l) = (\c' -> DynLayerColor c' l) <$> f c

instance IsNode (DynLayerColor t) where
    toNode (DynLayerColor _ l) = toNode l

instance IsLayer (DynLayerColor t) where
    toLayer (DynLayerColor _ l) = toLayer l

layerColor :: NodeGraph t m => LayerColorConfig t -> m (DynLayerColor t)
layerColor conf = do
    l <- createLayerColor
    appNodeConfig conf l
    appColorConfig conf l
    askParent >>= flip addChild l
    return $ DynLayerColor conf l

layerColor_ :: NodeGraph t m => LayerColorConfig t -> m ()
layerColor_ = void . layerColor

-- * Various helper functions

appDyn :: NodeGraph t m => (a -> HostFrame t ()) -> Dynamic t a -> m ()
appDyn setter dyn = do
    schedulePostBuild $ setter =<< sample (current dyn)
    forH_ (updated dyn) $ setter

setV :: Monad m => (n -> Double -> m ()) -> (n -> Double -> m ()) -> (n -> V2 Double -> m ())
setV setX setY n (V2 x y) = setX n x >> setY n y

appBaseConfig :: (IsNode n, NodeGraph t m, HasBaseConfig c t) => c -> n -> m ()
appBaseConfig c n = do
    let BaseConfig pos anchor skew zIndex rotation scale visible opacity cascadeColor cascadeOpacity = c^.baseConfig
    appDyn (setV setX setY n) pos
    appDyn (setV setAnchorX setAnchorY n) anchor
    appDyn (setV setSkewX setSkewY n) skew
    appDyn (setZIndex n) zIndex
    appDyn (setV setRotationX setRotationY n) rotation
    appDyn (setV setScaleX setScaleY n) scale
    appDyn (setVisible n) visible
    appDyn (setOpacity n) opacity
    setCascadeColor n cascadeColor
    setCascadeOpacity n cascadeOpacity

appColorConfig :: (IsNode n, NodeGraph t m, HasColorConfig c t) => c -> n -> m ()
appColorConfig c n = appDyn (setColor n) (c^.color)

appSizeConfig :: (IsNode n, NodeGraph t m, HasSizeConfig c t) => c -> n -> m ()
appSizeConfig c n = appDyn (setV setWidth setHeight n) (c^.size)

appNodeConfig :: (IsNode n, NodeGraph t m, HasNodeConfig c t) => c -> n -> m ()
appNodeConfig c n = appBaseConfig c n >> appSizeConfig c n

-- XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double
