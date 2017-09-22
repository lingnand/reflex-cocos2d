{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Builder.CocoStudio
  ( CSLoaderException(..)
  , loadNodeFromCS
  , addNodeFromCS
  , loadSpriteFromCS
  , addSpriteFromCS
  , loadWidgetFromCS
  , addWidgetFromCS
  , loadNodeOfVisibleSizeFromCS
  , addNodeOfVisibleSizeFromCS
  ) where

import Data.Typeable
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Reader

import Graphics.UI.Cocos2d (CppPtr(..))
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Sprite
import Graphics.UI.Cocos2d.Widget
import Graphics.UI.Cocos2d.CocoStudio

import Reflex.Cocos2d.Builder.Class
import Reflex.Cocos2d.Finalize.Class
import Reflex.Cocos2d.FastTriggerEvent.Class

import Reflex.Cocos2d.Attributes.Base
import Reflex.Cocos2d.Builder.Node
import Reflex.Cocos2d.Builder.Widget

data CSLoaderException = CSLoaderException { csloaderFilename :: String } deriving (Show, Typeable)

instance Exception CSLoaderException

loadNodeFromCS' :: MonadIO m => (Node -> a) -> String -> m a
loadNodeFromCS' convert filename = liftIO $ do
    n <- csLoader_createNode filename
    when (n == nullptr) $
      throw $ CSLoaderException filename
    return $ convert n

-- | Directly add the node into the graph
addNodeFromCS' ::
  ( NodePtr a
  , MonadIO m, NodeBuilder t m
  , MonadFinalize m, MonadIO (Finalizable m) )
  => (Node -> a) -> String -> [Prop a m] -> m a
addNodeFromCS' convert filename = addNewChild (loadNodeFromCS' convert filename)

loadNodeFromCS :: MonadIO m => String -> m Node
loadNodeFromCS = loadNodeFromCS' id

addNodeFromCS ::
  ( MonadIO m, NodeBuilder t m
  , MonadFinalize m, MonadIO (Finalizable m) )
  => String -> [Prop Node m] -> m Node
addNodeFromCS = addNodeFromCS' id

loadSpriteFromCS :: MonadIO m => String -> m Sprite
loadSpriteFromCS = loadNodeFromCS' downToSprite

addSpriteFromCS ::
  ( MonadIO m, NodeBuilder t m
  , MonadFinalize m, MonadIO (Finalizable m) )
  => String -> [Prop Sprite m] -> m Sprite
addSpriteFromCS = addNodeFromCS' downToSprite

loadWidgetFromCS :: MonadIO m => String -> m Widget
loadWidgetFromCS = loadNodeFromCS' downToWidget

-- | also add the convenience of pulling the widgetEvents
addWidgetFromCS
  :: ( MonadIO m, NodeBuilder t m
     , MonadFinalize m, MonadIO (Finalizable m)
     , FastTriggerEvent t m )
  => String -> [Prop Widget m] -> m (Widget, WidgetEvents t)
addWidgetFromCS filename props = do
    w <- addNodeFromCS' downToWidget filename props
    evts <- getWidgetEvents w
    return (w, evts)

-- of visible size

loadNodeOfVisibleSizeFromCS :: MonadIO m => String -> m Node
loadNodeOfVisibleSizeFromCS filename = liftIO $ do
  n <- csLoader_createNodeOfVisibleSize filename
  when (n == nullptr) $
    throw $ CSLoaderException filename
  return n

addNodeOfVisibleSizeFromCS ::
  ( MonadIO m, NodeBuilder t m )
  => String -> m Node
addNodeOfVisibleSizeFromCS filename = do
    n <- loadNodeOfVisibleSizeFromCS filename
    getParent >>= liftIO . flip node_addChild n
    return n
