{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node
    (
      NodeConfig
    , LayerConfig
    , LayerColorConfig
    , HasTrans(..)
    , HasBaseConfig(..)
    , HasSizeConfig(..)
    , HasColorConfig(..)
    , HasNodeConfig(..)
    , HasLayerConfig(..)
    , HasLayerColorConfig(..)
    , HasSpriteConfig(..)
    , DynNode
    , DynLayer
    , DynLayerColor
    , DynSprite
    , node
    , node_
    , layer
    , layer_
    , layerColor
    , layerColor_
    , sprite
    , sprite_
    -- * re-export the lower level
    , convertToNodeSpace
    , convertToWorldSpace
    ) where

import Data.Default
import Control.Monad
import Control.Lens hiding (flipped)
import Diagrams hiding (size)
import Reflex
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import JavaScript.Cocos2d.Sprite
import Reflex.Trans
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Utils

-- * Node
data NodeConfig t = NodeConfig { _nToBaseConfig :: BaseConfig t
                               , _nToSizeConfig :: SizeConfig t
                               }
makeLenses ''NodeConfig

class (HasBaseConfig c t, HasSizeConfig c t) => HasNodeConfig c t | c -> t where
    nodeConfig :: Lens' c (NodeConfig t)

instance {-# OVERLAPPABLE #-} HasNodeConfig c t => HasBaseConfig c t where
    baseConfig = nodeConfig . nToBaseConfig

instance {-# OVERLAPPABLE #-} HasNodeConfig c t => HasSizeConfig c t where
    sizeConfig = nodeConfig . nToSizeConfig

instance HasNodeConfig (NodeConfig t) t where
    nodeConfig = id

instance Reflex t => Default (NodeConfig t) where
    def = NodeConfig def def

appNodeConfig :: (IsNode n, NodeGraph t m, HasNodeConfig c t) => c -> n -> m ()
appNodeConfig c n = appBaseConfig c n >> appSizeConfig c n


data DynNode t = DynNode (NodeConfig t) Node

instance HasNodeConfig (DynNode t) t where
    nodeConfig f (DynNode c n) = (\c' -> DynNode c' n) <$> f c

instance IsNode (DynNode t) where
    toNode (DynNode _ n) = n

node :: NodeGraph t m => NodeConfig t -> m (DynNode t)
node conf = do
    n <- createNode
    appNodeConfig conf n
    askParent >>= flip addChild n
    return $ DynNode conf n

node_ :: NodeGraph t m => NodeConfig t -> m ()
node_ = void . node

-- * Layer
newtype LayerConfig t = LayerConfig { _lToNodeConfig :: NodeConfig t }
makeLenses ''LayerConfig

class HasNodeConfig c t => HasLayerConfig c t | c -> t where
    layerConfig :: Lens' c (LayerConfig t)

instance {-# OVERLAPPABLE #-} HasLayerConfig c t => HasNodeConfig c t where
    nodeConfig = layerConfig . lToNodeConfig

instance HasLayerConfig (LayerConfig t) t where
    layerConfig = id

-- XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double

instance Reflex t => Default (LayerConfig t) where
    def = LayerConfig $ def & anchor .~ constDyn (pure 0.5)
                            & size .~ constDyn (V2 winWidth winHeight)


data DynLayer t = DynLayer (LayerConfig t) Layer

instance HasLayerConfig (DynLayer t) t where
    layerConfig f (DynLayer c l) = (\c' -> DynLayer c' l) <$> f c

instance IsNode (DynLayer t) where
    toNode (DynLayer _ l) = toNode l

instance IsLayer (DynLayer t) where
    toLayer (DynLayer _ l) = l

layer :: NodeGraph t m => LayerConfig t -> m (DynLayer t)
layer conf = do
    l <- createLayer
    appNodeConfig conf l
    askParent >>= flip addChild l
    return $ DynLayer conf l

layer_ :: NodeGraph t m => LayerConfig t -> m ()
layer_ = void . layer


-- * LayerColor
data LayerColorConfig t = LayerColorConfig { _lcToLayerConfig :: LayerConfig t
                                           , _lcToColorConfig :: ColorConfig t
                                           }
makeLenses ''LayerColorConfig

class (HasLayerConfig c t, HasColorConfig c t) => HasLayerColorConfig c t | c -> t where
    layerColorConfig :: Lens' c (LayerColorConfig t)

instance {-# OVERLAPPABLE #-} HasLayerColorConfig c t => HasLayerConfig c t where
    layerConfig = layerColorConfig . lcToLayerConfig

instance HasColorConfig (LayerColorConfig t) t where
    colorConfig = lcToColorConfig

instance HasLayerColorConfig (LayerColorConfig t) t where
    layerColorConfig = id

instance Reflex t => Default (LayerColorConfig t) where
    def = LayerColorConfig def def


data DynLayerColor t = DynLayerColor (LayerColorConfig t) LayerColor

instance HasColorConfig (DynLayerColor t) t where
    colorConfig = layerColorConfig . lcToColorConfig

instance HasLayerColorConfig (DynLayerColor t) t where
    layerColorConfig f (DynLayerColor c l) = (\c' -> DynLayerColor c' l) <$> f c

instance IsNode (DynLayerColor t) where
    toNode (DynLayerColor _ l) = toNode l

instance IsLayer (DynLayerColor t) where
    toLayer (DynLayerColor _ l) = toLayer l

layerColor :: NodeGraph t m => LayerColorConfig t -> m (DynLayerColor t)
layerColor conf = do
    l <- createLayerColor
    appNodeConfig conf l
    appColorConfig conf l
    askParent >>= flip addChild l
    return $ DynLayerColor conf l

layerColor_ :: NodeGraph t m => LayerColorConfig t -> m ()
layerColor_ = void . layerColor

-- * Sprite
data SpriteConfig t = SpriteConfig { _sToBaseConfig :: BaseConfig t
                                   , _sToColorConfig :: ColorConfig t
                                   , _sToFlipped :: Dynamic t (V2 Bool)
                                   , _sToSpriteName :: Dynamic t String
                                   }
makeLenses ''SpriteConfig

class (HasBaseConfig c t, HasColorConfig c t) => HasSpriteConfig c t | c -> t where
    spriteConfig :: Lens' c (SpriteConfig t)
    spriteName :: Lens' c (Dynamic t String)
    spriteName = spriteConfig . sToSpriteName
    flipped :: Lens' c (Dynamic t (V2 Bool))
    flipped = spriteConfig . sToFlipped

instance HasBaseConfig (SpriteConfig t) t where
    baseConfig = sToBaseConfig

instance HasColorConfig (SpriteConfig t) t where
    colorConfig = sToColorConfig

instance HasSpriteConfig (SpriteConfig t) t where
    spriteConfig = id

instance Reflex t => Default (SpriteConfig t) where
    def = SpriteConfig { _sToBaseConfig = def & anchor .~ constDyn (pure 0.5)
                       , _sToColorConfig = def
                       , _sToFlipped = constDyn $ pure False
                       , _sToSpriteName = constDyn ""
                       }


data DynSprite t = DynSprite (SpriteConfig t) Sprite

instance HasBaseConfig (DynSprite t) t where
    baseConfig = spriteConfig . sToBaseConfig

instance HasColorConfig (DynSprite t) t where
    colorConfig = spriteConfig . sToColorConfig

instance HasSpriteConfig (DynSprite t) t where
    spriteConfig f (DynSprite c s) = (\c' -> DynSprite c' s) <$> f c

instance IsNode (DynSprite t) where
    toNode (DynSprite _ s) = toNode s

instance IsSprite (DynSprite t) where
    toSprite (DynSprite _ s) = s

sprite :: NodeGraph t m => SpriteConfig t -> m (DynSprite t)
sprite conf = do
    s <- createSprite
    appBaseConfig conf s
    appColorConfig conf s
    appDyn (setV setFlippedX setFlippedY s) (conf^.flipped)
    let setSprite [] = return ()
        setSprite name = setSpriteByName s name
    appDyn setSprite (conf^.spriteName)
    askParent >>= flip addChild s
    return $ DynSprite conf s

sprite_ :: NodeGraph t m => SpriteConfig t -> m ()
sprite_ = void . sprite
