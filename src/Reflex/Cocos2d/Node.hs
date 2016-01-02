{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node (
    NodeConfig(NodeConfig),
    LayerConfig(LayerConfig),
    LayerColorConfig(LayerColorConfig),
    HasNodeConfig(..),
    HasLayerConfig(..),
    HasLayerColorConfig(..),
    node,
    node_,
    layer,
    layer_,
    layerColor,
    layerColor_
) where

import Data.Colour
import Data.Colour.Names
import Data.Default
import Control.Monad.IO.Class
import Control.Lens
import Linear
import Reflex
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import Reflex.Cocos2d.Class

data NodeConfig t
   = NodeConfig { _position :: Dynamic t (V2 Double)
                , _size :: Dynamic t (V2 Double) -- | size as a vector (width: x, height: y)
                , _anchor :: Dynamic t (V2 Double)
                , _skew :: Dynamic t (V2 Double)
                , _zIndex :: Dynamic t Int
                , _rotation :: Dynamic t (V2 Double)
                , _scale :: Dynamic t (V2 Double)
                , _visible :: Dynamic t Bool
                , _color :: Dynamic t (Colour Double)
                , _opacity :: Dynamic t Double -- | 0.0 - 1.0
                , _cascadeColor :: Bool
                , _cascadeOpacity :: Bool
                }
makeClassy ''NodeConfig

newtype LayerConfig t = LayerConfig (NodeConfig t)

instance HasNodeConfig (LayerConfig t) t where
    nodeConfig f (LayerConfig c) = (\c' -> LayerConfig c') <$> f c

class HasNodeConfig c t => HasLayerConfig c t where
    layerConfig :: Lens' c (LayerConfig t)

instance HasLayerConfig (LayerConfig t) t where
    layerConfig = id

newtype LayerColorConfig t = LayerColorConfig (LayerConfig t)

instance HasNodeConfig (LayerColorConfig t) t where
    nodeConfig f (LayerColorConfig (LayerConfig c)) = (\c' -> LayerColorConfig $ LayerConfig c') <$> f c

instance HasLayerConfig (LayerColorConfig t) t where
    layerConfig f (LayerColorConfig c) = (\c' -> LayerColorConfig c') <$> f c

class HasLayerConfig c t => HasLayerColorConfig c t where
    layerColorConfig :: Lens' c (LayerColorConfig t)

instance HasLayerColorConfig (LayerColorConfig t) t where
    layerColorConfig = id

instance Reflex t => Default (NodeConfig t) where
    def = NodeConfig { _position = constDyn zero
                     , _size = constDyn zero
                     , _anchor = constDyn zero
                     , _skew = constDyn zero
                     , _zIndex = constDyn 0
                     , _rotation = constDyn zero
                     , _scale = constDyn $ pure 1.0
                     , _visible = constDyn True
                     , _color = constDyn white
                     , _opacity = constDyn 1.0
                     , _cascadeColor = False
                     , _cascadeOpacity = False
                     }

instance Reflex t => Default (LayerConfig t) where
    def = LayerConfig $ def & anchor .~ constDyn (pure 0.5) 
                            & size .~ constDyn (V2 winWidth winHeight)

instance Reflex t => Default (LayerColorConfig t) where
    def = LayerColorConfig $ def & color .~ constDyn black

node :: NodeGraph t m => NodeConfig t -> m a -> m a
node conf child = liftIO createNode >>= \n -> integrate n conf child

node_ :: NodeGraph t m => NodeConfig t -> m ()
node_ conf = node conf (return ())

layer :: NodeGraph t m => LayerConfig t -> m a -> m a
layer conf child = liftIO createLayer >>= \l -> integrate l (conf ^. nodeConfig) child

layer_ :: NodeGraph t m => LayerConfig t -> m ()
layer_ conf = layer conf (return ())

layerColor :: NodeGraph t m => LayerColorConfig t -> m a -> m a
layerColor conf child = liftIO createLayerColor >>= \l -> integrate l (conf ^. nodeConfig) child

layerColor_ :: NodeGraph t m => LayerColorConfig t -> m ()
layerColor_ conf = layerColor conf (return ())

applyNodeConfig :: (IsNode n, NodeGraph t m) => n -> NodeConfig t -> m ()
applyNodeConfig n (NodeConfig pos size anchor skew zIndex rotation scale visible color opacity cascadeColor cascadeOpacity) = do
    -- schedule post all values that requires lazy input
    let app setter dyn = do
            schedulePostBuild $ liftIO . setter n =<< sample (current dyn)
            addVoidAction $ liftIO . setter n <$> updated dyn
        vset xset yset n (V2 x y) = xset n x >> yset n y 
    app (vset setX setY) pos
    app (vset setWidth setHeight) size 
    app (vset setAnchorX setAnchorY) anchor
    app (vset setSkewX setSkewY) skew
    app setZIndex zIndex
    app (vset setRotationX setRotationY) rotation
    app (vset setScaleX setScaleY) scale
    app setVisible visible
    app setColor color
    app setOpacity opacity
    liftIO $ do
        setCascadeColor n cascadeColor
        setCascadeOpacity n cascadeOpacity

integrate :: (NodeGraph t m, IsNode n) => n -> NodeConfig t -> m a -> m a
integrate n conf child = do
    applyNodeConfig n conf
    p <- askParent 
    liftIO $ addChild p n
    subGraph (toNode n) child

-- | XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double
