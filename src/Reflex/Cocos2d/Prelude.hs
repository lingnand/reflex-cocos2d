module Reflex.Cocos2d.Prelude
    (
      module Reflex.Cocos2d
    , module Reflex.Trans
    , module Data.Time.Clock
    , module Diagrams.Prelude
    , module Diagrams.TwoD.Vector
    , module Diagrams.BoundingBox
    , module JavaScript.Cocos2d
    , module Math.Probable
    )
  where

import Diagrams.Prelude hiding (arrow, loc, position, sample, opacity, _opacity, scale, size, flipped, Dynamic, End, text, stroke, fontSize, normal, discrete)
import Diagrams.TwoD.Vector (e)
import Diagrams.BoundingBox
import Data.Time.Clock
import Reflex.Trans
import Reflex.Cocos2d
import JavaScript.Cocos2d
import Math.Probable hiding (Event, never, EventT, P, prob, liftF {- conflict with Free -})
