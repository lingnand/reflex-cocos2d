{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Class where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Exception
import Control.Monad.Trans.Free
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

class ( ReflexHost t, MonadIO m, MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      , MonadAsyncException m, MonadAsyncException (HostFrame t)) => NodeGraph t m where
    askParent :: m Node
    -- | Schedule an action to occur after the current cohort has been
    -- built; this is necessary because Behaviors built in the current
    -- cohort may not be read until after it is complete
    askPostBuildEvent :: m (Event t ())
    askRunWithActions :: m ([DSum (EventTrigger t) Identity] -> IO ())

    -- Composition primitives
    -- | Run a graph under a given node
    subG :: IsNode n => n -> m a -> m a
    -- | Run a graph with the initial content and the updated content
    -- whenever the event updates
    holdG :: IsNode n => n -> m a -> Event t (m b) -> m (a, Event t b)
    -- | sequencing
    sequenceG :: Event t (m a) -> m (Event t a)
    sequenceG_ :: Event t (m a) -> m ()
    sequenceG_ = void . sequenceG
    -- lower in power (no construction power, but performance-wise better)
    -- this is mostly used inside the library to improve performance
    sequenceH_ :: Event t (HostFrame t ()) -> m ()
    -- misc operations
    -- | Generate a new Event that delays the input Event by some frame
    -- (normally fired in the immediate next frame); similar to
    -- setTimeout(0)
    delay :: Event t a -> m (Event t a)

-- Mappings
-- | HostFrame mappings
sequenceGMaybe :: NodeGraph t m => Event t (m (Maybe a)) -> m (Event t a)
sequenceGMaybe = return . fmapMaybe id <=< sequenceG

mapGMaybe :: NodeGraph t m => (a -> m (Maybe b)) -> Event t a -> m (Event t b)
mapGMaybe f = sequenceGMaybe . (f <$>)

mapG :: NodeGraph t m => (a -> m b) -> Event t a -> m (Event t b)
mapG f = sequenceG . (f <$>)

mapG_ :: NodeGraph t m => (a -> m b) -> Event t a -> m ()
mapG_ f = sequenceG_ . (f <$>)

forGMaybe :: NodeGraph t m => Event t a -> (a -> m (Maybe b)) -> m (Event t b)
forGMaybe = flip mapGMaybe

forG :: NodeGraph t m => Event t a -> (a -> m b) -> m (Event t b)
forG = flip mapG

forG_ :: NodeGraph t m => Event t a -> (a -> m b) -> m ()
forG_ = flip mapG_

filterG :: NodeGraph t m => (a -> m Bool) -> Event t a -> m (Event t a)
filterG f = mapGMaybe $ \a -> do
  valid <- f a
  return $ guard valid >> return a


-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 =|
(=|) :: (NodeGraph t m, IsNode n) => m n -> m a -> m (n, a)
(=|) node child = do
    n <- node
    a <- subG n child
    return (n, a)

-- | Hold
-- e.g., @newChild & nodeBuilder -| child0@
infixr 2 -|
(-|) :: (NodeGraph t m, IsNode n) => m n -> m a -> Event t (m a) -> m (n, Dynamic t a)
(-|) node child0 newChild = do
    n <- node
    (result0, newResult) <- holdG n child0 newChild
    dyn <- holdDyn result0 newResult
    return (n, dyn)

-- | Recursive hold
infixr 2 -<<
(-<<) :: (NodeGraph t m, IsNode n) => m n -> FreeT (Event t) m a -> m n
(-<<) node ft = mdo
    let toMEvt (FreeT mf) = mf >>= \case
              Pure _ -> return never
              Free k -> return k
        newChild = toMEvt <$> switchPromptlyDyn dyns
    (n, dyns) <- node -| toMEvt ft $ newChild
    return n

-- | View
-- e.g., @nodeBuilder =| child@
infixr 2 -<
(-<) :: (NodeGraph t m, IsNode n) => m n -> Dynamic t (m a) -> m (n, Event t a)
(-<) node child = do
    n <- node
    e <- askPostBuildEvent
    let newChild = leftmost [updated child, tag (current child) e]
    (_, evt) <- holdG n (return ()) newChild
    return (n, evt)
