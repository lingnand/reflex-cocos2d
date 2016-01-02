{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node (
    NodeConfig(NodeConfig),
    position,
    size,
    anchor,
    skew,
    zIndex,
    rotation,
    scale,
    visible,
    color,
    opacity,
    cascadeColor,
    cascadeOpacity,
    node,
    node_,
    layer,
    layer_
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

-- the n here is a free type variable that can be specialized to different
-- IsNode types
data NodeConfig n t
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
makeLenses ''NodeConfig

instance Reflex t => Default (NodeConfig Node t) where
    def = NodeConfig { _position = constDyn zero
                     , _size = constDyn $ V2 winWidth winHeight
                     , _anchor = constDyn $ pure 0.5
                     , _skew = constDyn zero
                     , _zIndex = constDyn 0
                     , _rotation = constDyn zero
                     , _scale = constDyn $ pure 1.0
                     , _visible = constDyn False
                     , _color = constDyn white
                     , _opacity = constDyn 1.0
                     , _cascadeColor = False
                     , _cascadeOpacity = False
                     }

instance Reflex t => Default (NodeConfig Layer t) where
    def = NodeConfig { _position = constDyn zero
                     , _size = constDyn zero
                     , _anchor = constDyn zero
                     , _skew = constDyn zero
                     , _zIndex = constDyn 0
                     , _rotation = constDyn zero
                     , _scale = constDyn $ pure 1.0
                     , _visible = constDyn False
                     , _color = constDyn white
                     , _opacity = constDyn 1.0
                     , _cascadeColor = False
                     , _cascadeOpacity = False
                     }

node :: NodeGraph t m => NodeConfig Node t -> m a -> m a
node conf child = liftIO createNode >>= \n -> integrate n conf child

node_ :: NodeGraph t m => NodeConfig Node t -> m ()
node_ conf = node conf (return ())

layer :: NodeGraph t m => NodeConfig Layer t -> m a -> m a
layer conf child = liftIO createLayer >>= \l -> integrate l conf child

layer_ :: NodeGraph t m => NodeConfig Layer t -> m ()
layer_ conf = layer conf (return ())

applyNodeConfig :: (IsNode n, NodeGraph t m) => n -> NodeConfig n t -> m ()
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

integrate :: (NodeGraph t m, IsNode n) => n -> NodeConfig n t -> m a -> m a
integrate n conf child = do
    applyNodeConfig n conf
    p <- askParent 
    liftIO $ addChild p n
    subGraph (toNode n) child

-- | XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double
