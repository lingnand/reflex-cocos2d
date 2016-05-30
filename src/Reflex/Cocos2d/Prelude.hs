module Reflex.Cocos2d.Prelude
    (
      module Reflex.Cocos2d
    , module Data.Time.Clock
    , module Diagrams.Prelude
    , module Diagrams.TwoD.Vector
    , module Diagrams.BoundingBox
    , module JavaScript.Cocos2d
    , module Math.Probable
    , module System.Random.MWC
    , module Control.Monad.Primitive
    )
  where

import Diagrams.Prelude hiding
  ( arrow, loc, position, sample, opacity, _opacity
  , scale, scaleX, scaleY, size, flipped, width, height
  , Dynamic, End, text, stroke, fontSize, normal, discrete
  , set)
import Diagrams.TwoD.Vector (e)
import Diagrams.BoundingBox
import Data.Time.Clock
import Reflex.Cocos2d
import JavaScript.Cocos2d
import Math.Probable hiding (Event, never, EventT, P, prob, liftF {- conflict with Free -})
import System.Random.MWC (createSystemRandom, Gen, GenIO)
import Control.Monad.Primitive

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

instance MonadTrans RandT where
    lift = RandT . const

instance MonadIO m => MonadIO (RandT m) where
    liftIO = lift . liftIO
