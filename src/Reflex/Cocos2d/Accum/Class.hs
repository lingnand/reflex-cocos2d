{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Accum.Class
    (
      MonadAccum(..)
    )
  where

import Control.Monad.Trans
import Control.Monad.Fix

import Reflex

-- | Dual of MonadAdjust - instead of running replacement on new events, accumulate the effects
class (Reflex t, Monad m) => MonadAccum t m | m -> t where
    runWithAccumulation :: m a -> Event t (m b) -> m (a, Event t b)

instance (MonadFix m, MonadHold t m, MonadAccum t m) => MonadAccum t (PostBuildT t m) where
    -- similar to the MonadAdjust implementation
    runWithAccumulation z em = do
      postBuild <- getPostBuild
      lift $ do
        rec result@(_, result') <- runWithAccumulation (runPostBuildT z postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') em
            let voidResult' = fmapCheap (const ()) result'
        return result
