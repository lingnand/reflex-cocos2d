{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Attributes.Node
  ( anchor
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
  , textureRect
  , flipped
  , flippedX
  , flippedY
  ) where

import Data.Colour
import Control.Monad
import Control.Monad.Trans
import Control.Lens ((^.))

import Diagrams (Point(..), V2(..), (@@), deg)
import Graphics.UI.Cocos2d (Decodable(..), nullptr, Size(..), Rect)
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Sprite
import Graphics.UI.Cocos2d.Texture
import Graphics.UI.Cocos2d.Common

import Reflex.Cocos2d.Attributes.Base

---- Various Attributes ----
instance {-# OVERLAPPABLE #-} (MonadIO m, NodePtr n) => HasRWPositionAttrib n m where
  position = hoistA liftIO $ Attrib getPosition setPosition
    where
      getPosition = liftIO . fmap P . (decode <=< node_getPosition)
      setPosition n (P v2) = liftIO $ node_setPosition n v2
  positionX = hoistA liftIO $ Attrib node_getPositionX node_setPositionX
  positionY = hoistA liftIO $ Attrib node_getPositionY node_setPositionY

-- | cocos2d uses clockwise degree - we convert it to anticlockwise angle
instance {-# OVERLAPPABLE #-} (MonadIO m, NodePtr n) => HasRWAngleAttrib n m where
  angle = hoistA liftIO $ Attrib getter setter
    where
      fromCC = (@@ deg) . negate
      toCC = negate . (^. deg)
      getter = liftIO . fmap fromCC . node_getRotation
      setter n = node_setRotation n . toCC

instance {-# OVERLAPPABLE #-} (MonadIO m, NodePtr n) => HasROPositionAttrib n m where
  roPosition = ROAttrib $ getter position

instance {-# OVERLAPPABLE #-} (MonadIO m, NodePtr n) => HasROAngleAttrib n m where
  roAngle = ROAttrib $ getter angle

-- | anchor expressed as percentage: V2 ([0-1], [0-1])
anchor :: (MonadIO m, NodePtr n) => Attrib' n m (V2 Float)
anchor = hoistA liftIO $ Attrib (decode <=< node_getAnchorPoint) node_setAnchorPoint

anchorX :: (MonadIO m, NodePtr n) => Attrib' n m Float
anchorX = hoistA liftIO $ Attrib getter setter
  where
    getter = vec2_x_get <=< node_getAnchorPoint
    setter n x = do
      y <- vec2_y_get =<< node_getAnchorPoint n
      node_setAnchorPoint n (V2 x y)

anchorY :: (MonadIO m, NodePtr n) => Attrib' n m Float
anchorY = hoistA liftIO $ Attrib getter setter
  where
    getter = vec2_y_get <=< node_getAnchorPoint
    setter n y = do
      x <- vec2_x_get =<< node_getAnchorPoint n
      node_setAnchorPoint n (V2 x y)

skew :: (MonadIO m, NodePtr n) => Attrib' n m (V2 Float)
skew = hoistA liftIO $ Attrib getter setter
  where
    getter n = V2 <$> node_getSkewX n <*> node_getSkewY n
    setter n (V2 x y) = node_setSkewX n x >> node_setSkewY n y

skewX :: (MonadIO m, NodePtr n) => Attrib' n m Float
skewX = hoistA liftIO $ Attrib node_getSkewX node_setSkewX

skewY :: (MonadIO m, NodePtr n) => Attrib' n m Float
skewY = hoistA liftIO $ Attrib node_getSkewY node_setSkewY

zOrder :: (MonadIO m, NodePtr n) => Attrib' n m Int
zOrder = hoistA liftIO $ Attrib node_getLocalZOrder node_setLocalZOrder

scale :: (MonadIO m, NodePtr n) => Attrib' n m (V2 Float)
scale = hoistA liftIO $ Attrib getter setter
  where
    getter n = V2 <$> node_getScaleX n <*> node_getScaleY n
    setter n (V2 x y) = node_setScaleX n x >> node_setScaleY n y

scaleX :: (MonadIO m, NodePtr n) => Attrib' n m Float
scaleX = hoistA liftIO $ Attrib node_getScaleX node_setScaleX

scaleY :: (MonadIO m, NodePtr n) => Attrib' n m Float
scaleY = hoistA liftIO $ Attrib node_getScaleY node_setScaleY

visible :: (MonadIO m, NodePtr n) => Attrib' n m Bool
visible = hoistA liftIO $ Attrib node_isVisible node_setVisible

-- | Color; mostly only useful for LayerColor & Sprite
color :: (MonadIO m, NodePtr n) => Attrib' n m (Colour Float)
color = hoistA liftIO $ Attrib (decode <=< node_getColor) node_setColor

nodeGetOpacityInFloat :: NodePtr n => n -> IO Float
nodeGetOpacityInFloat n = (/ 255) . fromIntegral <$> node_getOpacity n

nodeSetOpacityInFloat :: NodePtr n => n -> Float -> IO ()
nodeSetOpacityInFloat n a = node_setOpacity n (round $ a * 255)

opacity :: (MonadIO m, NodePtr n) => Attrib' n m Float
opacity = hoistA liftIO $ Attrib nodeGetOpacityInFloat nodeSetOpacityInFloat

alphaColor :: (MonadIO m, NodePtr n) => Attrib' n m (AlphaColour Float)
alphaColor = hoistA liftIO $ Attrib getter setter
  where
    getter n =
      withOpacity <$> (node_getColor n >>= decode) <*> nodeGetOpacityInFloat n
    setter n ac = do
      let a = alphaChannel ac
          c
            | a > 0 = darken (recip a) (ac `over` black)
            | otherwise = black
      node_setColor n c >> nodeSetOpacityInFloat n a

cascadeColor :: (MonadIO m, NodePtr n) => Attrib' n m Bool
cascadeColor = hoistA liftIO $ Attrib node_isCascadeColorEnabled node_setCascadeColorEnabled

cascadeOpacity :: (MonadIO m, NodePtr n) => Attrib' n m Bool
cascadeOpacity = hoistA liftIO $ Attrib node_isCascadeOpacityEnabled node_setCascadeOpacityEnabled

-- | Content size; not useful for Sprite
contentSize :: (MonadIO m, NodePtr n) => Attrib' n m (Size Float)
contentSize = hoistA liftIO $ Attrib (decode <=< node_getContentSize) node_setContentSize

width :: (MonadIO m, NodePtr n) => Attrib' n m Float
width = hoistA liftIO $ Attrib getter setter
  where
    getter = rawSize_width_get <=< node_getContentSize
    setter n w = do
      h <- rawSize_height_get =<< node_getContentSize n
      node_setContentSize n (S $ V2 w h)

height :: (MonadIO m, NodePtr n) => Attrib' n m Float
height = hoistA liftIO $ Attrib getter setter
  where
    getter = rawSize_height_get <=< node_getContentSize
    setter n h = do
      w <- rawSize_width_get =<< node_getContentSize n
      node_setContentSize n (S $ V2 w h)

texture :: (MonadIO m, SpritePtr n) => WOAttrib' n m Texture2D
texture = WOAttrib $ \sp -> liftIO . sprite_setTexture sp

-- | Currently modelled as non-stoppable action that gets run when set
-- action :: (MonadIO m, NodePtr n) => WOAttrib' n m Action
-- action = WOAttrib' runAction
-- | Set SpriteFrame by name.
-- NOTE: the SpriteFrame has to be already inside the SpriteFrameCache
-- spriteName :: (MonadIO m, SpritePtr n) => WOAttrib' n m String
-- spriteName = WOAttrib' $ \sp -> liftIO . sprite_setSpriteFrameWithName sp
-- | Set texture by its filename
-- NOTE: this automatically adds the texture to the texture cache if it's not already there
textureFilename :: (MonadIO m, SpritePtr n) => WOAttrib' n m String
textureFilename = WOAttrib $ \sp name -> liftIO $ case name of
                    [] -> sprite_setTexture sp (nullptr :: Texture2D) -- reset sprite to blank
                    _ -> sprite_setTextureWithFilename sp name

textureRect :: (MonadIO m, SpritePtr n) => WOAttrib' n m (Rect Float)
textureRect = WOAttrib $ \sp rect -> liftIO $ sprite_setTextureRect sp rect

flipped :: (MonadIO m, SpritePtr n) => Attrib' n m (V2 Bool)
flipped = hoistA liftIO $ Attrib getter setter
  where
    getter n = V2 <$> sprite_isFlippedX n <*> sprite_isFlippedY n
    setter n (V2 x y) = sprite_setFlippedX n x >> sprite_setFlippedY n y

flippedX :: (MonadIO m, SpritePtr n) => Attrib' n m Bool
flippedX = hoistA liftIO $ Attrib sprite_isFlippedX sprite_setFlippedX

flippedY :: (MonadIO m, SpritePtr n) => Attrib' n m Bool
flippedY = hoistA liftIO $ Attrib sprite_isFlippedY sprite_setFlippedY
