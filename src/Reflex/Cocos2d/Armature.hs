module Reflex.Cocos2d.Armature
  (
    armature
  , animationSpeedScale
  , animation
  ) where

import Control.Monad.IO.Class
import JavaScript.Cocos2d.Armature
import JavaScript.Cocos2d.Node
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Attributes

armature :: NodeGraph t m => String -> [Prop Armature m] -> m Armature
armature name props = do
    arm <- createArmature name
    set arm props
    askParent >>= flip addChild arm
    return arm

animationSpeedScale :: MonadIO m  => Attrib Armature m Double
animationSpeedScale = attrib getArmatureAnimationSpeedScale setArmatureAnimationSpeedScale

animation :: MonadIO m => SetOnlyAttrib Armature m (String, Bool)
animation = SetOnlyAttrib' (uncurry . playArmatureAnimation)
