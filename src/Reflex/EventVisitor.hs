-- | Provides 'EventVisitorT', which exposes a metalanguage for structuring nested events
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.EventVisitor
  (
    EventVisitorT
  , runEventVisitorT
  , runEventVisitorT_

  , exec
  , wait
  , wait'
  , wait_
  , waitDynMaybe
  , waitDynMaybe'
  , waitDynMaybe_
  , waitDynBool
  ) where

import Data.Bool
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Free

import Reflex
import Reflex.Extra

newtype EventVisitorT t m a = EventVisitorT { unEventVisitorT :: FreeT (Event t) m a }

deriving instance (Functor (Event t), Monad m) => Functor (EventVisitorT t m)
deriving instance (Functor (Event t), Monad m) => Applicative (EventVisitorT t m)
deriving instance (Functor (Event t), Monad m) => Monad (EventVisitorT t m)

runEventVisitorT
  :: forall t m a.
     (MonadFix m , PostBuild t m , MonadHold t m, MonadAdjust t m)
  => EventVisitorT t m a -> m (Event t a)
runEventVisitorT (EventVisitorT ft) = do
    rec (result0, newResult) <- runWithReplace (runFreeT ft) (runFreeT <$> newFs)
        let startE = case result0 of
              Pure _ -> never :: Event t (FreeT (Event t) m a)
              Free e -> e
        newFs <- switchPromptly startE (fmapMaybe previewFree newResult)
    case result0 of
      Pure a -> postpone a
      _ -> return $ fmapMaybe previewPure newResult

runEventVisitorT_
  :: (MonadFix m , PostBuild t m , MonadHold t m, MonadAdjust t m)
  => EventVisitorT t m a -> m ()
runEventVisitorT_ = void . runEventVisitorT

-- | Execute an action in the current context
exec :: Monad m => m a -> EventVisitorT t m a
exec = EventVisitorT . lift

-- | Wait until an event is fired and return the value for the first occurrence
wait :: (Reflex t, Monad m) => Event t a -> EventVisitorT t m a
wait = EventVisitorT . liftF

-- | Wait for the first occurrence and include the future occurrences in return
wait' :: (Reflex t, Monad m) => Event t a -> EventVisitorT t m (a, Event t a)
wait' e = (,e) <$> wait e

wait_ :: (Reflex t, Monad m) => Event t a -> EventVisitorT t m ()
wait_ = void . wait

-- | Wait for the Dynamic to turn from Nothing to Just
waitDynMaybe :: (Reflex t, MonadSample t m, PostBuild t m) => Dynamic t (Maybe a) -> EventVisitorT t m a
waitDynMaybe dyn = EventVisitorT $ lift (sample $ current dyn) >>= \case
    -- schedule the next section immediately to postbuild
    Just a -> lift (postpone a) >>= liftF
    _ -> liftF $ fmapMaybe id (updated dyn)

-- | Wait for the first Just value, and include the future values in return
waitDynMaybe' :: (Reflex t, MonadSample t m, PostBuild t m) => Dynamic t (Maybe a) -> EventVisitorT t m (a, Event t a)
waitDynMaybe' dyn = (,fmapMaybe id $ updated dyn) <$> waitDynMaybe dyn

waitDynMaybe_ :: (Reflex t, MonadSample t m, PostBuild t m) => Dynamic t (Maybe a) -> EventVisitorT t m ()
waitDynMaybe_ = void . waitDynMaybe

waitDynBool :: (Reflex t, MonadSample t m, PostBuild t m) => Dynamic t Bool -> EventVisitorT t m ()
waitDynBool = waitDynMaybe . fmap (bool (Just ()) Nothing)

previewFree :: FreeF f a b -> Maybe (f b)
previewFree (Free fb) = Just fb
previewFree _ = Nothing

previewPure :: FreeF f a b -> Maybe a
previewPure (Pure a) = Just a
previewPure _ = Nothing
