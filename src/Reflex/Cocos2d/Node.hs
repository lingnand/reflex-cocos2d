{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Cocos2d.Node
    ( node
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
    , zOrder
    , scale
    , scaleX
    , scaleY
    , visible
    , color
    , opacity
    , alphaColor
    , cascadeColor
    , cascadeOpacity
    , contentSize
    , width
    , height
    -- , action
    , texture
    , textureFilename
    , flipped
    , flippedX
    , flippedY
    -- utils --
    , getChildByName
    , getChildSpriteByName
    , getChildButtonByName
    , getChildTextByName
    , getChildLayoutByName
    -- helper --
    , addNewChild
    -- re-export --
    , Node
    , Layer
    , LayerColor
    , Sprite
    , Texture2D

    , NodePtr(..)
    , LayerPtr
    , LayerColorPtr
    , SpritePtr
    )
  where

import Data.Colour
import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding (flipped, over)
import Diagrams (Point(..), V2(..), Angle, (@@), deg, _theta)
import Diagrams.Direction
import Diagrams.TwoD.Vector
import Graphics.UI.Cocos2d.Common
import Graphics.UI.Cocos2d.Texture
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Layer
import Graphics.UI.Cocos2d.Widget

-- import Graphics.UI.Cocos2d.Action
import Graphics.UI.Cocos2d.Sprite
import Foreign.Hoppy.Runtime (Decodable(..), CppPtr(..))
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes

-- * Node
addNewChild :: (NodeGraph t m, NodePtr n) => IO n -> [Prop n m] -> m n
addNewChild factory props = do
  n <- liftIO factory
  setProps n props
  view parent >>= liftIO . flip node_addChild n
  return n

node :: NodeGraph t m => [Prop Node m] -> m Node
node = addNewChild node_create

node_ :: NodeGraph t m => [Prop Node m] -> m ()
node_ = void . node

layer :: NodeGraph t m => [Prop Layer m] -> m Layer
layer = addNewChild layer_create

layer_ :: NodeGraph t m => [Prop Layer m] -> m ()
layer_ = void . layer

layerColor :: NodeGraph t m => [Prop LayerColor m] -> m LayerColor
layerColor = addNewChild layerColor_create

layerColor_ :: NodeGraph t m => [Prop LayerColor m] -> m ()
layerColor_ = void . layerColor

-- * Sprite

sprite :: NodeGraph t m => [Prop Sprite m] -> m Sprite
sprite = addNewChild sprite_create

sprite_ :: NodeGraph t m => [Prop Sprite m] -> m ()
sprite_ = void . sprite


---- Various Attributes ----
instance (MonadIO m, NodePtr n) => HasPosition n m where
  position = hoistA liftIO $ Attrib' getPosition setPosition
    where
      getPosition = liftIO . fmap P . (decode <=< node_getPosition)
      setPosition n (P v2) = liftIO $ node_setPosition n v2
  positionX = hoistA liftIO $ Attrib' node_getPositionX node_setPositionX
  positionY = hoistA liftIO $ Attrib' node_getPositionY node_setPositionY

-- | Convert rotation angle (CCW) to direction
eDir :: Floating n => Angle n -> Direction V2 n
eDir = dir . e

-- | cocos2d uses clockwise degree - we convert it to agnostic Direction (where xDir is *without*
-- any rotation)
instance (MonadIO m, NodePtr n) => HasRotation n m where
  rotation = hoistA liftIO $ Attrib' getter setter
    where
      fromCC = eDir . (@@ deg) . negate
      toCC = negate . (^. _theta . deg)
      getter = liftIO . fmap fromCC . node_getRotation
      setter n = node_setRotation n . toCC

-- | anchor expressed as percentage: V2 ([0-1], [0-1])
anchor :: (MonadIO m, NodePtr n) => Attrib n m (V2 Float)
anchor = hoistA liftIO $ Attrib' (decode <=< node_getAnchorPoint) node_setAnchorPoint

anchorX :: (MonadIO m, NodePtr n) => Attrib n m Float
anchorX = hoistA liftIO $ Attrib' getter setter
  where
    getter = vec2_x_get <=< node_getAnchorPoint
    setter n x = do
      y <- vec2_y_get =<< node_getAnchorPoint n
      node_setAnchorPoint n (V2 x y)

anchorY :: (MonadIO m, NodePtr n) => Attrib n m Float
anchorY = hoistA liftIO $ Attrib' getter setter
  where
    getter = vec2_y_get <=< node_getAnchorPoint
    setter n y = do
      x <- vec2_x_get =<< node_getAnchorPoint n
      node_setAnchorPoint n (V2 x y)

skew :: (MonadIO m, NodePtr n) => Attrib n m (V2 Float)
skew = hoistA liftIO $ Attrib' getter setter
  where
    getter n = V2 <$> node_getSkewX n <*> node_getSkewY n
    setter n (V2 x y) = node_setSkewX n x >> node_setSkewY n y

skewX :: (MonadIO m, NodePtr n) => Attrib n m Float
skewX = hoistA liftIO $ Attrib' node_getSkewX node_setSkewX

skewY :: (MonadIO m, NodePtr n) => Attrib n m Float
skewY = hoistA liftIO $ Attrib' node_getSkewY node_setSkewY

zOrder :: (MonadIO m, NodePtr n) => Attrib n m Int
zOrder = hoistA liftIO $ Attrib' node_getLocalZOrder node_setLocalZOrder

scale :: (MonadIO m, NodePtr n) => Attrib n m (V2 Float)
scale = hoistA liftIO $ Attrib' getter setter
  where
    getter n = V2 <$> node_getScaleX n <*> node_getScaleY n
    setter n (V2 x y) = node_setScaleX n x >> node_setScaleY n y

scaleX :: (MonadIO m, NodePtr n) => Attrib n m Float
scaleX = hoistA liftIO $ Attrib' node_getScaleX node_setScaleX

scaleY :: (MonadIO m, NodePtr n) => Attrib n m Float
scaleY = hoistA liftIO $ Attrib' node_getScaleY node_setScaleY

visible :: (MonadIO m, NodePtr n) => Attrib n m Bool
visible = hoistA liftIO $ Attrib' node_isVisible node_setVisible

-- | Color; mostly only useful for LayerColor & Sprite
color :: (MonadIO m, NodePtr n) => Attrib n m (Colour Float)
color = hoistA liftIO $ Attrib' (decode <=< node_getColor) node_setColor

node_getOpacityInFloat :: NodePtr n => n -> IO Float
node_getOpacityInFloat n = (/ 255) . fromIntegral <$> node_getOpacity n

node_setOpacityInFloat :: NodePtr n => n -> Float -> IO ()
node_setOpacityInFloat n a = node_setOpacity n (round $ a * 255)

opacity :: (MonadIO m, NodePtr n) => Attrib n m Float
opacity = hoistA liftIO $ Attrib' node_getOpacityInFloat node_setOpacityInFloat

alphaColor :: (MonadIO m, NodePtr n) => Attrib n m (AlphaColour Float)
alphaColor = hoistA liftIO $ Attrib' getter setter
  where
    getter n =
      withOpacity <$> (node_getColor n >>= decode) <*> node_getOpacityInFloat n
    setter n ac = do
      let a = alphaChannel ac
          c
            | a > 0 = darken (recip a) (ac `over` black)
            | otherwise = black
      node_setColor n c >> node_setOpacityInFloat n a

cascadeColor :: (MonadIO m, NodePtr n) => Attrib n m Bool
cascadeColor = hoistA liftIO $ Attrib' node_isCascadeColorEnabled node_setCascadeColorEnabled

cascadeOpacity :: (MonadIO m, NodePtr n) => Attrib n m Bool
cascadeOpacity = hoistA liftIO $ Attrib' node_isCascadeOpacityEnabled node_setCascadeOpacityEnabled

-- | Content size; not useful for Sprite
contentSize :: (MonadIO m, NodePtr n) => Attrib n m (V2 Float)
contentSize = hoistA liftIO $ Attrib' (decode <=< node_getContentSize) node_setContentSize

width :: (MonadIO m, NodePtr n) => Attrib n m Float
width = hoistA liftIO $ Attrib' getter setter
  where
    getter = size_width_get <=< node_getContentSize
    setter n w = do
      h <- size_height_get =<< node_getContentSize n
      node_setContentSize n (V2 w h)

height :: (MonadIO m, NodePtr n) => Attrib n m Float
height = hoistA liftIO $ Attrib' getter setter
  where
    getter = size_height_get <=< node_getContentSize
    setter n h = do
      w <- size_width_get =<< node_getContentSize n
      node_setContentSize n (V2 w h)

texture :: (MonadIO m, SpritePtr n) => SetOnlyAttrib n m Texture2D
texture = SetOnlyAttrib' $ \sp -> liftIO . sprite_setTexture sp

-- | Currently modelled as non-stoppable action that gets run when set
-- action :: (MonadIO m, NodePtr n) => SetOnlyAttrib n m Action
-- action = SetOnlyAttrib runAction
-- | Set SpriteFrame by name.
-- NOTE: the SpriteFrame has to be already inside the SpriteFrameCache
-- spriteName :: (MonadIO m, SpritePtr n) => SetOnlyAttrib n m String
-- spriteName = SetOnlyAttrib $ \sp -> liftIO . sprite_setSpriteFrameWithName sp
-- | Set texture by its filename
-- NOTE: this automatically adds the texture to the texture cache if it's not already there
textureFilename :: (MonadIO m, SpritePtr n) => SetOnlyAttrib n m String
textureFilename = SetOnlyAttrib' $ \sp name -> liftIO $ case name of
                    [] -> sprite_setTexture sp (nullptr :: Texture2D) -- reset sprite to blank
                    _ -> sprite_setTextureWithFilename sp name

flipped :: (MonadIO m, SpritePtr n) => Attrib n m (V2 Bool)
flipped = hoistA liftIO $ Attrib' getter setter
  where
    getter n = V2 <$> sprite_isFlippedX n <*> sprite_isFlippedY n
    setter n (V2 x y) = sprite_setFlippedX n x >> sprite_setFlippedY n y

flippedX :: (MonadIO m, SpritePtr n) => Attrib n m Bool
flippedX = hoistA liftIO $ Attrib' sprite_isFlippedX sprite_setFlippedX

flippedY :: (MonadIO m, SpritePtr n) => Attrib n m Bool
flippedY = hoistA liftIO $ Attrib' sprite_isFlippedY sprite_setFlippedY

----- Utils
getChildByName' :: (MonadIO m, NodeValue n) => (Node -> a) -> n -> String -> m (Maybe a)
getChildByName' convert n name =
  liftIO $
  do n <- node_getChildByName n name
     if n == nullptr
       then return Nothing
       else return . Just $ convert n

getChildByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Node)
getChildByName = getChildByName' id

getChildSpriteByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Sprite)
getChildSpriteByName = getChildByName' downToSprite

getChildButtonByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Button)
getChildButtonByName = getChildByName' downToButton

getChildTextByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Text)
getChildTextByName = getChildByName' downToText

getChildLayoutByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Layout)
getChildLayoutByName = getChildByName' downToLayout
