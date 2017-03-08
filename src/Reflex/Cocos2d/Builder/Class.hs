{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Cocos2d.Builder.Class
    ( Time
    , NodeBuilder(..)
    , (-<)
    )
  where

import Diagrams (V2)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Reflex
import Reflex.State
import Graphics.UI.Cocos2d.Node

type Time = Float -- ^ in seconds

class (Reflex t, Monad m) => NodeBuilder t m | m -> t where
    getParent :: m Node
    -- | Execute an action with a different parent
    withParent :: Node -> m a -> m a

    getWindowSize :: m (V2 Float)
    -- | Ticks for each frame
    getFrameTicks :: m (Event t Time)

instance NodeBuilder t m => NodeBuilder t (PostBuildT t m) where
    getParent = lift getParent
    withParent n m = do
      pb <- getPostBuild
      lift $ withParent n (runPostBuildT m pb)
    getWindowSize = lift getWindowSize
    getFrameTicks = lift getFrameTicks

instance NodeBuilder t m => NodeBuilder t (AccStateT t f s m) where
    getParent = lift getParent
    withParent n m = restoreT . return =<< liftWith (\run -> withParent n (run m))
    getWindowSize = lift getWindowSize
    getFrameTicks = lift getFrameTicks

-- * Compositions
-- | Embed
-- e.g., @nodeBuilder -< child@
infixr 2 -<
(-<) :: (NodePtr n, NodeBuilder t m)
     => m n -> m a -> m (n, a)
(-<) node child = do
    n <- node
    a <- withParent (toNode n) child
    return (n, a)
