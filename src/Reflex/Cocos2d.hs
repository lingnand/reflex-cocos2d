{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Reflex.Cocos2d
  (
    module X
  , CocosBuilder
  ) where

import Reflex.Cocos2d.Finalize.Base as X
import Reflex.Cocos2d.Finalize.Class as X
import Reflex.Cocos2d.Builder.Base as X
import Reflex.Cocos2d.Builder.Class as X
import Reflex.Cocos2d.Builder.Node as X
import Reflex.Cocos2d.Builder.Label as X
import Reflex.Cocos2d.Builder.Widget as X
import Reflex.Cocos2d.Builder.CocoStudio as X
import Reflex.Cocos2d.Accum.Class as X
import Reflex.Cocos2d.Attributes.Base as X
import Reflex.Cocos2d.Attributes.Node as X
import Reflex.Cocos2d.FastTriggerEvent.Class as X
import Reflex.Cocos2d.Misc.Audio as X
import Reflex.Cocos2d.Misc.Chipmunk as X
import Reflex.Cocos2d.Misc.Logging as X
import Reflex.Cocos2d.Event as X
import Reflex.Cocos2d.Types as X
import Reflex.Cocos2d.Decomposable as X
import Reflex.Cocos2d.Internal  as X

import qualified Control.Monad.Fix as F
import qualified Control.Monad.Trans as T
import qualified Reflex as R

-- | Convenience constraint synonym for a cocos builder monad
type CocosBuilder t m =
  ( X.NodeBuilder t m
  , F.MonadFix m, R.MonadHold t m
  , R.PostBuild t m, X.MonadAccum t m, R.MonadAdjust t m
  , T.MonadIO m, X.FastTriggerEvent t m, R.TriggerEvent t m
  , X.MonadFinalize m, T.MonadIO (X.Finalizable m)
  , R.PerformEvent t m, T.MonadIO (R.Performable m)
  )
