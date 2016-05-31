{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node
    (
      node
    , node_
    , layer
    , layer_
    , layerColor
    , layerColor_
    , sprite
    , sprite_
    -- attrs --
    , eDir
    , rot
    , anchor
    , anchorX
    , anchorY
    , skew
    , skewX
    , skewY
    , zIndex
    , scale
    , scaleX
    , scaleY
    , visible
    , opacity
    , cascadeColor
    , cascadeOpacity
    , size
    , width
    , height
    , color
    , action
    , spriteName
    , flipped
    , flippedX
    , flippedY
    -- * re-export the lower level
    , IsNode(..), IsLayer(..), IsSprite(..)
    , Node, Layer, LayerColor, Sprite
    , convertToNodeSpace
    , convertToWorldSpace
    , getChildByName
    , castNode
    ) where

import Data.Colour
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens hiding (flipped, set)
import Diagrams hiding (sample, opacity, _opacity, scale, scaleX, scaleY, size, getOpacity, width, height)
import Diagrams.Direction
import Diagrams.TwoD.Vector
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import JavaScript.Cocos2d.Action
import JavaScript.Cocos2d.Sprite
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes

-- * Node
node :: NodeGraph t m => [Prop Node m] -> m Node
node props = do
    n <- createNode
    set n props
    askParent >>= flip addChild n
    return n

node_ :: NodeGraph t m => [Prop Node m] -> m ()
node_ = void . node

layer :: NodeGraph t m => [Prop Layer m] -> m Layer
layer props = do
    l <- createLayer
    set l props
    askParent >>= flip addChild l
    return l

layer_ :: NodeGraph t m => [Prop Layer m] -> m ()
layer_ = void . layer

layerColor :: NodeGraph t m => [Prop LayerColor m] -> m LayerColor
layerColor props = do
    l <- createLayerColor
    set l props
    askParent >>= flip addChild l
    return l

layerColor_ :: NodeGraph t m => [Prop LayerColor m] -> m ()
layerColor_ = void . layerColor

-- * Sprite

sprite :: NodeGraph t m => [Prop Sprite m] -> m Sprite
sprite props = do
    s <- createSprite
    set s props
    askParent >>= flip addChild s
    return s

sprite_ :: NodeGraph t m => [Prop Sprite m] -> m ()
sprite_ = void . sprite


---- Various Attributes ----

instance (MonadIO m, IsNode n) => HasPosition n m where
  pos = attrib getPosition setPosition
  posX = attrib getX setX
  posY = attrib getY setY

-- | Convert rotation angle (CCW) to direction
eDir :: Floating n => Angle n -> Direction V2 n
eDir = dir . e

-- | cocos2d uses clockwise degree - we convert it to agnostic Direction (where xDir is *without*
-- any rotation)
instance (MonadIO m, IsNode n) => HasRotation n m where
  rot = attrib getter setter
    where fromCC = eDir . (@@ deg) . negate
          toCC = negate . (^._theta.deg)
          getter = fmap fromCC . getRotation
          setter n = setRotation n . toCC

-- | anchor expressed as percentage: V2 ([0-1], [0-1])
anchor :: (MonadIO m, IsNode n) => Attrib n m (V2 Double)
anchor = attrib getAnchor setAnchor

anchorX :: (MonadIO m, IsNode n) => Attrib n m Double
anchorX = attrib getAnchorX setAnchorX

anchorY :: (MonadIO m, IsNode n) => Attrib n m Double
anchorY = attrib getAnchorY setAnchorY

skew :: (MonadIO m, IsNode n) => Attrib n m (V2 Double)
skew = attrib getSkew setSkew

skewX :: (MonadIO m, IsNode n) => Attrib n m Double
skewX = attrib getSkewX setSkewX

skewY :: (MonadIO m, IsNode n) => Attrib n m Double
skewY = attrib getSkewY setSkewY

zIndex :: (MonadIO m, IsNode n) => Attrib n m Int
zIndex = attrib getZIndex setZIndex

scale :: (MonadIO m, IsNode n) => Attrib n m (V2 Double)
scale = attrib getScale setScale

scaleX :: (MonadIO m, IsNode n) => Attrib n m Double
scaleX = attrib getScaleX setScaleX

scaleY :: (MonadIO m, IsNode n) => Attrib n m Double
scaleY = attrib getScaleY setScaleY

visible :: (MonadIO m, IsNode n) => Attrib n m Bool
visible = attrib getVisible setVisible

opacity :: (MonadIO m, IsNode n) => Attrib n m Double
opacity = attrib getOpacity setOpacity

cascadeColor :: (MonadIO m, IsNode n) => Attrib n m Bool
cascadeColor = attrib getCascadeColor setCascadeColor

cascadeOpacity :: (MonadIO m, IsNode n) => Attrib n m Bool
cascadeOpacity = attrib getCascadeOpacity setCascadeOpacity

-- | Content size; not useful for Sprite
size :: (MonadIO m, IsNode n) => Attrib n m (V2 Double)
size = attrib getSize setSize

width :: (MonadIO m, IsNode n) => Attrib n m Double
width = attrib getWidth setWidth

height :: (MonadIO m, IsNode n) => Attrib n m Double
height = attrib getHeight setHeight

-- | Color; mostly only useful for LayerColor & Sprite
color :: (MonadIO m, IsNode n) => Attrib n m (Colour Double)
color = attrib getColor setColor

-- | Currently modelled as non-stoppable action that gets run when set
action :: (MonadIO m, IsNode n) => SetOnlyAttrib n m Action
action = SetOnlyAttrib runAction

-- | Set the sprite's frame name. (can only get textureFileName once set, which /= frameName)
spriteName :: (MonadIO m, IsSprite n) => SetOnlyAttrib n m String
spriteName = SetOnlyAttrib setSpriteByName

flipped :: (MonadIO m, IsSprite n) => Attrib n m (V2 Bool)
flipped = attrib getFlipped setFlipped

flippedX :: (MonadIO m, IsSprite n) => Attrib n m Bool
flippedX = attrib getFlippedX setFlippedX

flippedY :: (MonadIO m, IsSprite n) => Attrib n m Bool
flippedY = attrib getFlippedY setFlippedY
