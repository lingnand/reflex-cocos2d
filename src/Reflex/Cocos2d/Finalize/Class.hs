{-# LANGUAGE TypeFamilies #-}
-- | The class that adds Finalizable actions that should be triggered when the existing effects are
-- 'undone' (i.e., when MonadAdjust is triggered)
module Reflex.Cocos2d.Finalize.Class
  (
    MonadFinalize(..)
  ) where

import Control.Monad.Trans
import Reflex.PostBuild.Base
import Reflex.State

class Monad m => MonadFinalize m where
    type Finalizable m :: * -> *
    addFinalizer :: Finalizable m () -> m ()

instance MonadFinalize m => MonadFinalize (PostBuildT t m) where
    type Finalizable (PostBuildT t m) = Finalizable m
    addFinalizer = lift . addFinalizer

instance MonadFinalize m => MonadFinalize (AccStateT t f s m) where
    type Finalizable (AccStateT t f s m) = Finalizable m
    {-# INLINABLE addFinalizer #-}
    addFinalizer = lift . addFinalizer
