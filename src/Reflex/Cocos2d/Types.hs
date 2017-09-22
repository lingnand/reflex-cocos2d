{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Cocos2d.Types
    ( Mouse(Mouse)
    , mouseCursorLocation
    , mouseScroll
    , mouseButton
    , Touch(Touch)
    , touchLocation
    , Acceleration(Acceleration)
    , accelerationVector
    , accelerationTimestamp

    , Outline(Outline)
    , outlineColor
    , outlineSize
    , Shadow(Shadow)
    , shadowColor
    , shadowOffset
    , shadowBlur
    , Glow(Glow)
    , glowColor

    -- re-export cocos2d-hs types
    , Size(..)
    , size_width
    , size_height

    , Rect(..)
    , rect_origin
    , rect_size
    )
  where

import Data.Default
import Diagrams (Point(..), P2, V2(..), V3(..))
import Control.Lens
import Data.Colour
import Data.Colour.Names

import Graphics.UI.Cocos2d (Decodable(..))
import Graphics.UI.Cocos2d.Event
import Graphics.UI.Cocos2d.Extra

data Mouse = Mouse
    { _mouseCursorLocation :: P2 Float
    , _mouseScroll         :: V2 Float
    , _mouseButton         :: Int
    }

mouseButton :: Lens' Mouse Int
mouseButton f (Mouse loc scroll but)
  = fmap
      (\ but' -> Mouse loc scroll but') (f but)
{-# INLINE mouseButton #-}
mouseCursorLocation :: Lens' Mouse (P2 Float)
mouseCursorLocation f (Mouse loc scroll but)
  = fmap
      (\ loc' -> Mouse loc' scroll but) (f loc)
{-# INLINE mouseCursorLocation #-}
mouseScroll :: Lens' Mouse (V2 Float)
mouseScroll f (Mouse loc scroll but)
  = fmap
      (\ scroll' -> Mouse loc scroll' but) (f scroll)
{-# INLINE mouseScroll #-}

instance Decodable EventMouse Mouse where
  decode = decode . toEventMouseConst

instance Decodable EventMouseConst Mouse where
  decode em = Mouse <$> (P <$> eventMouse_getLocation em)
                    <*> (V2 <$> eventMouse_getScrollX em <*> eventMouse_getScrollY em)
                    <*> eventMouse_getMouseButton em

data Touch = Touch
    { _touchLocation :: P2 Float
    }

touchLocation :: Lens' Touch (P2 Float)
touchLocation f (Touch loc) = fmap Touch (f loc)
{-# INLINE touchLocation #-}

instance Decodable EventTouch Touch where
  decode = decode . toEventTouchConst

instance Decodable EventTouchConst Touch where
  decode et = Touch . P <$> eventTouch_getLocation et

data Acceleration = Acceleration
    { _accelerationVector    :: V3 Double
    , _accelerationTimestamp :: Double
    }

accelerationTimestamp :: Lens' Acceleration Double
accelerationTimestamp f (Acceleration vec ts)
  = fmap
      (\ ts' -> Acceleration vec ts') (f ts)
{-# INLINE accelerationTimestamp #-}
accelerationVector :: Lens' Acceleration (V3 Double)
accelerationVector f (Acceleration vec ts)
  = fmap
      (\ vec' -> Acceleration vec' ts) (f vec)
{-# INLINE accelerationVector #-}

instance Decodable EventAcceleration Acceleration where
  decode = decode . toEventAccelerationConst

instance Decodable EventAccelerationConst Acceleration where
  decode ea = Acceleration <$> (V3 <$> eventAcceleration_x_get ea
                                   <*> eventAcceleration_y_get ea
                                   <*> eventAcceleration_z_get ea)
                           <*> eventAcceleration_timestamp_get ea

-- Label
data Outline = Outline
    { _outlineColor :: AlphaColour Float
    , _outlineSize  :: Int
    } deriving (Show, Eq)

outlineColor :: Lens' Outline (AlphaColour Float)
outlineColor f (Outline color size)
  = fmap (\ color' -> Outline color' size) (f color)
{-# INLINE outlineColor #-}
outlineSize :: Lens' Outline Int
outlineSize f (Outline color size)
  = fmap (\ size' -> Outline color size') (f size)
{-# INLINE outlineSize #-}

instance Default Outline where
  def = Outline
      { _outlineColor = opaque white
      , _outlineSize  = 0
      }

data Shadow = Shadow
    { _shadowColor  :: AlphaColour Float
    , _shadowOffset :: V2 Float
    , _shadowBlur   :: Int
    } deriving (Show, Eq)

shadowBlur :: Lens' Shadow Int
shadowBlur f (Shadow color offset blur)
  = fmap
      (\ blur' -> Shadow color offset blur')
      (f blur)
{-# INLINE shadowBlur #-}
shadowColor :: Lens' Shadow (AlphaColour Float)
shadowColor f (Shadow color offset blur)
  = fmap
      (\ color' -> Shadow color' offset blur)
      (f color)
{-# INLINE shadowColor #-}
shadowOffset :: Lens' Shadow (V2 Float)
shadowOffset f (Shadow color offset blur)
  = fmap
      (\ offset' -> Shadow color offset' blur)
      (f offset)
{-# INLINE shadowOffset #-}

instance Default Shadow where
    def = Shadow
        { _shadowColor  = white `withOpacity` 0.5
        , _shadowOffset = 0
        , _shadowBlur   = 0
        }

data Glow = Glow
    { _glowColor  :: AlphaColour Float
    } deriving (Show, Eq)

glowColor :: Lens' Glow (AlphaColour Float)
glowColor f (Glow color) = fmap Glow (f color)
{-# INLINE glowColor #-}

instance Default Glow where
    def = Glow
        { _glowColor = white `withOpacity` 0.5
        }
