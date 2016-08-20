{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Class
  (
    NodeGraph(..)
  , (-<)
  , (-|)
  , (-<<)
  , (<-<)
  , pushPostBuild
  , buildDyn
  , buildDyn'
  , runDyn
  , buildF
  , switchFT
  , runF
  ) where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Trans.Free
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

class ( ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m )
      => NodeGraph t m where
    askParent :: m Node
    -- | Schedule an action to occur after the current cohort has been
    -- built; this is necessary because Behaviors built in the current
    -- cohort may not be read until after it is complete
    askPostBuildEvent :: m (Event t ())
    askRunWithActions :: m (([DSum (EventTrigger t) Identity], IO ()) -> IO ())

    -- Composition primitives
    -- | Run a graph under a given node
    subGraph :: IsNode n => n -> m a -> m a
    -- | Run a graph with the initial content and the updated content
    -- whenever the event updates
    holdGraph :: IsNode n => n -> m a -> Event t (m b) -> m (a, Event t b)
    -- | sequencing
    buildEvent :: Event t (m a) -> m (Event t a)
    buildEvent_ :: Event t (m a) -> m ()

    -- misc operations
    -- lower in power (no construction power, but performance-wise better)
    -- this is mostly used inside the library to improve performance
    runEventMaybe :: Event t (HostFrame t (Maybe a)) -> m (Event t a)
    runEvent_ :: Event t (HostFrame t ()) -> m ()
    runEvent :: Event t (HostFrame t a) -> m (Event t a)

    filterMEvent :: (a -> HostFrame t Bool) -> Event t a -> m (Event t a)
    filterMEvent f e = runEventMaybe . ffor e $ \v -> do
                            b <- f v
                            return $ guard b >> return v
    onEventMaybe :: Event t a -> (a -> HostFrame t (Maybe b)) -> m (Event t b)
    onEventMaybe e = runEventMaybe . ffor e
    onEvent :: Event t a -> (a -> HostFrame t b) -> m (Event t b)
    onEvent e = runEvent . ffor e
    onEvent_ :: Event t a -> (a -> HostFrame t ()) -> m ()
    onEvent_ e = runEvent_ . ffor e

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 -<
(-<) :: (NodeGraph t m, IsNode n) => m n -> m a -> m (n, a)
(-<) node child = do
    n <- node
    a <- subGraph n child
    return (n, a)

-- | Hold
-- e.g., @newChild & nodeBuilder -| child0@
infixr 2 -|
(-|) :: (NodeGraph t m, IsNode n) => m n -> m a -> Event t (m a) -> m (n, Dynamic t a)
(-|) node child0 newChild = do
    n <- node
    (result0, newResult) <- holdGraph n child0 newChild
    dyn <- holdDyn result0 newResult
    return (n, dyn)

-- | View
-- e.g., @nodeBuilder -< child@
infixr 2 -<<
(-<<) :: (NodeGraph t m, IsNode n) => m n -> Dynamic t (m a) -> m (n, Event t a)
(-<<) node child = do
    n <- node
    (_, evt) <- holdGraph n (return ()) =<< pushPostBuild child
    return (n, evt)

-- | Free
infixr 2 <-<
(<-<) :: (NodeGraph t m, IsNode n) => m n -> FreeT (Event t) m a -> m (n, Event t a)
(<-<) node ft = mdo
    n <- node
    pe <- askPostBuildEvent
    let startE = case result0 of
          Pure a -> return a <$ pe
          Free e -> e
    newFs <- switchPromptly startE $ fmapMaybe previewFree newResult
    (result0, newResult) <- holdGraph n (runFreeT ft) (runFreeT <$> newFs)
    return (n, fmapMaybe previewPure newResult)

-- | Convert to an Event that carries the first value in postBuild
pushPostBuild :: NodeGraph t m => Dynamic t a -> m (Event t a)
pushPostBuild d = do
    pe <- askPostBuildEvent
    return $ leftmost [ pushAlways (const $ sample (current d)) pe
                      , updated d ]

-- lazy version of building dyn
buildDyn :: NodeGraph t m => Dynamic t (m a) -> m (Event t a)
buildDyn = buildEvent <=< pushPostBuild

-- strict version
buildDyn' :: NodeGraph t m => Dynamic t (m a) -> m (Dynamic t a)
buildDyn' d = do
    a <- join $ sample (current d)
    evt <- buildEvent (updated d)
    holdDyn a evt

runDyn :: NodeGraph t m => Dynamic t (HostFrame t a) -> m (Event t a)
runDyn = runEvent <=< pushPostBuild

buildF :: NodeGraph t m => FreeT (Event t) m a -> m (Event t a)
buildF ft = runFreeT ft >>= \case
    Pure v -> fmap (v <$) askPostBuildEvent
    Free startE -> mdo
      newFs <- switchPromptly startE $ fmapMaybe previewFree e'
      e' <- buildEvent $ runFreeT <$> newFs
      return $ fmapMaybe previewPure e'

switchFT :: NodeGraph t m => FreeT (Event t) m a -> m (Event t a)
switchFT ft = switchFT' ft >>= \case
    Pure a -> fmap (a <$) askPostBuildEvent
    Free e -> return e

switchFT' :: NodeGraph t m => FreeT (Event t) m a -> m (FreeF (Event t) a a)
switchFT' ft = runFreeT ft >>= \case
    Pure v -> return $ Pure v
    Free e -> do
      e' <- buildEvent $ ffor e $ switchFT' >=> \case
                Pure a -> return $ a <$ e
                Free ie -> return ie
      Free <$> switchPromptly never e'


runF :: NodeGraph t m => FreeT (Event t) (HostFrame t) a -> m (Event t a)
runF ft = do
    pe <- askPostBuildEvent
    rec newFs <- switchPromptly (ft <$ pe) $ fmapMaybe previewFree e'
        e' <- runEvent $ runFreeT <$> newFs
    return $ fmapMaybe previewPure e'


previewPure :: FreeF f a b -> Maybe a
previewPure (Pure a) = Just a
previewPure _ = Nothing

previewFree :: FreeF f a b -> Maybe (f b)
previewFree (Free fb) = Just fb
previewFree _ = Nothing
