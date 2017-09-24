{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Cocos2d.Accum.Class
  (
    MonadAccum(..)
  , runWithAccumDyn'
  ) where

import Control.Monad.Trans
import Control.Monad.Fix

import Reflex

-- | Dual of MonadAdjust - instead of running replacement on new events, accumulate the effects
class (Reflex t, Monad m) => MonadAccum t m | m -> t where
    runWithAccum :: m a -> Event t (m b) -> m (a, Event t b)

runWithAccumDyn' :: (MonadHold t m, MonadAccum t m) => Dynamic t (m a) -> m (Dynamic t a)
runWithAccumDyn' d = do
  m0 <- sample (current d)
  (a0, ea) <- runWithAccum m0 (updated d)
  holdDyn a0 ea

instance (MonadFix m, MonadHold t m, MonadAccum t m) => MonadAccum t (PostBuildT t m) where
    -- similar to the MonadAdjust implementation
    runWithAccum z em = do
      postBuild <- getPostBuild
      lift $ do
        rec result@(_, result') <- runWithAccum (runPostBuildT z postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') em
            let voidResult' = fmapCheap (const ()) result'
        return result
