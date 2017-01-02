module Reflex.Cocos2d.CocoStudio
    ( CSLoaderException(..)
    , loadNodeFromCS
    , addNodeFromCS
    , loadSpriteFromCS
    , addSpriteFromCS
    , loadWidgetFromCS
    , addWidgetFromCS
    , loadNodeOfVisibleSizeFromCS
    , addNodeOfVisibleSizeFromCS
    )
  where

import Data.Typeable
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad
import Control.Lens

import Foreign.Hoppy.Runtime (CppPtr(..))
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Sprite
import Graphics.UI.Cocos2d.Widget
import Graphics.UI.Cocos2d.CocoStudio

import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Widget
import Reflex.Cocos2d.Attributes

data CSLoaderException = CSLoaderException { csloaderFilename :: String } deriving (Show, Typeable)

instance Exception CSLoaderException

loadNodeFromCS' :: MonadIO m => (Node -> a) -> String -> m a
loadNodeFromCS' convert filename = liftIO $ do
    n <- csLoader_createNode filename
    when (n == nullptr) $
      throw $ CSLoaderException filename
    return $ convert n

-- | Directly add the node into the graph
addNodeFromCS' :: (NodeBuilder t host m, NodePtr a)
               => (Node -> a) -> String -> [Prop a m] -> m a
addNodeFromCS' convert filename = addNewChild (loadNodeFromCS' convert filename)

loadNodeFromCS :: MonadIO m => String -> m Node
loadNodeFromCS = loadNodeFromCS' id

addNodeFromCS :: NodeBuilder t host m => String -> [Prop Node m] -> m Node
addNodeFromCS = addNodeFromCS' id

loadSpriteFromCS :: MonadIO m => String -> m Sprite
loadSpriteFromCS = loadNodeFromCS' downToSprite

addSpriteFromCS :: NodeBuilder t host m => String -> [Prop Sprite m] -> m Sprite
addSpriteFromCS = addNodeFromCS' downToSprite

loadWidgetFromCS :: MonadIO m => String -> m Widget
loadWidgetFromCS = loadNodeFromCS' downToWidget

-- | also add the convenience of pulling the widgetEvents
addWidgetFromCS
  :: NodeBuilder t host m
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

addNodeOfVisibleSizeFromCS :: NodeBuilder t host m => String -> m Node
addNodeOfVisibleSizeFromCS filename = do
    n <- loadNodeOfVisibleSizeFromCS filename
    view parent >>= liftIO . flip node_addChild n
    return n
