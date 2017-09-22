{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Cocos2d.Builder.Node
    ( node
    , node_
    , sprite
    , sprite_
    -- utils --
    , getChildByName
    , getChildSpriteByName
    , getChildButtonByName
    , getChildTextByName
    , getChildLayoutByName
    -- helper --
    , addNewChild
    -- re-export --
    , Node
    , Sprite
    , Texture2D

    , NodePtr(..)
    , SpritePtr
    )
  where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Cocos2d.Texture
import Graphics.UI.Cocos2d.Node
import Graphics.UI.Cocos2d.Widget

-- import Graphics.UI.Cocos2d.Action
import Graphics.UI.Cocos2d.Sprite
import Graphics.UI.Cocos2d (CppPtr(..))

import Reflex.Cocos2d.Builder.Class
import Reflex.Cocos2d.Finalize.Class
import Reflex.Cocos2d.Attributes.Base

-- * Node
addNewChild ::
  ( NodePtr n
  , MonadIO m, NodeBuilder t m
  , MonadFinalize m, MonadIO (Finalizable m) )
  => IO n -> [Prop n m] -> m n
addNewChild factory props = do
  n <- liftIO factory
  setProps n props
  p <- getParent
  liftIO $ node_addChild p n
  addFinalizer . liftIO $ do
    putStrLn "[debug] removing Node!"
    node_removeChild p n
  return n

node :: ( MonadIO m, NodeBuilder t m
        , MonadFinalize m, MonadIO (Finalizable m) )
     => [Prop Node m] -> m Node
node = addNewChild node_create

node_ :: ( MonadIO m, NodeBuilder t m
         , MonadFinalize m, MonadIO (Finalizable m) )
      => [Prop Node m] -> m ()
node_ = void . node

-- * Sprite

sprite :: ( MonadIO m, NodeBuilder t m
          , MonadFinalize m, MonadIO (Finalizable m) )
       => [Prop Sprite m] -> m Sprite
sprite = addNewChild sprite_create

sprite_ :: ( MonadIO m, NodeBuilder t m
           , MonadFinalize m, MonadIO (Finalizable m) )
        => [Prop Sprite m] -> m ()
sprite_ = void . sprite


----- Utils
getChildByName' :: (MonadIO m, NodeValue n) => (Node -> a) -> n -> String -> m (Maybe a)
getChildByName' convert n name =
  liftIO $
  do n <- node_getChildByName n name
     if n == nullptr
       then return Nothing
       else return . Just $ convert n

getChildByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Node)
getChildByName = getChildByName' id

getChildSpriteByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Sprite)
getChildSpriteByName = getChildByName' downToSprite

getChildButtonByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Button)
getChildButtonByName = getChildByName' downToButton

getChildTextByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Text)
getChildTextByName = getChildByName' downToText

getChildLayoutByName :: (MonadIO m, NodeValue n) => n -> String -> m (Maybe Layout)
getChildLayoutByName = getChildByName' downToLayout
