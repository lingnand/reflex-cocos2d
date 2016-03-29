{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- Abstraction over translatable and rotatable objects
module Reflex.Trans
    ( Trans(Trans)
    , HasTrans(..)
    , eDir
    )
  where

import Diagrams hiding (position, rotation)
import Diagrams.Direction
import Diagrams.TwoD.Vector
import Data.Default
import Control.Lens
import Reflex


data Trans t = Trans { _pos :: Dynamic t (P2 Double)
                       -- | represented as a Direction, where the
                       -- unrotated direction is xDir
                     , _rot :: Dynamic t (Direction V2 Double)
                     }
makeClassy ''Trans

-- | Convert rotation angle (CCW) to direction
eDir :: Floating n => Angle n -> Direction V2 n
eDir = dir . e

instance Reflex t => Default (Trans t) where
    def = Trans { _pos = constDyn 0
                , _rot = constDyn xDir
                }
