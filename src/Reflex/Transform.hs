{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- Abstraction over translatable and rotatable objects
module Reflex.Transform
    ( Transform(Transform)
    , HasTransform(..)
    )
  where

import Linear
import Data.Default
import Control.Lens hiding (transform)
import Reflex

data Transform t = Transform { _position :: Dynamic t (V2 Double)
                               -- | the CW angle in degrees
                             , _rotation :: Dynamic t Double
                             }
makeClassy ''Transform

instance Reflex t => Default (Transform t) where
    def = Transform { _position = constDyn 0
                    , _rotation = constDyn 0
                    }
