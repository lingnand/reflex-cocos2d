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
import Control.Monad.Trans.Free
import Control.Monad.Primitive
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node

class ( ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadHold t m
      , MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO
      , MonadReflexCreateTrigger t m, MonadSubscribeEvent t m
      -- XXX: is this good practice..?
      , PrimMonad m, PrimState m ~ PrimState IO)
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
    -- | Generate a new Event that delays the input Event by some frame
    -- (normally fired in the immediate next frame); similar to
    -- setTimeout(0)
    delay :: Event t a -> m (Event t a)
    -- buildEventMaybe :: NodeGraph t m => Event t (m (Maybe a)) -> m (Event t a)
    -- buildEventMaybe = return . fmapMaybe id <=< buildEvent
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
infixr 2 =|
(=|) :: (NodeGraph t m, IsNode n) => m n -> m a -> m (n, a)
(=|) node child = do
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
    (_, evt) <- holdGraph n (return ()) newChild
    return (n, evt)
