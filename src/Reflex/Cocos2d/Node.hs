{-# LANGUAGE FlexibleInstances #-}
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
import Data.Colour.Names
import Data.Dependent.Sum (DSum (..))
import Control.Monad
import Control.Monad.Ref
import Control.Lens hiding (flipped)
import Diagrams hiding (size)
import Reflex
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import JavaScript.Cocos2d.Sprite
import Reflex.Trans
import Reflex.Host.Class
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Utils
import Control.Monad.IO.Class

-- * Node
data NodeConfig t = NodeConfig { _nToBaseConfig :: BaseConfig t
                               , _nToSizeConfig :: SizeConfig t
                               }
makeLenses ''NodeConfig

class (HasBaseConfig c t, HasSizeConfig c t) => HasNodeConfig c t | c -> t where
    nodeConfig :: Lens' c (NodeConfig t)

instance HasNodeConfig (NodeConfig t) t where
    nodeConfig = id

instance HasBaseConfig (NodeConfig t) t where
    baseConfig = nToBaseConfig

instance HasTrans (NodeConfig t) t where
    trans = baseConfig . trans

instance HasSizeConfig (NodeConfig t) t where
    sizeConfig = nToSizeConfig

instance Reflex t => Default (NodeConfig t) where
    def = NodeConfig def def

appNodeConfig :: (IsNode n, NodeGraph t m, HasNodeConfig c t) => c -> n -> m ()
appNodeConfig c n = appBaseConfig c n >> appSizeConfig c n


data DynNode t = DynNode (NodeConfig t) Node

instance HasNodeConfig (DynNode t) t where
    nodeConfig f (DynNode c n) = (\c' -> DynNode c' n) <$> f c

instance HasBaseConfig (DynNode t) t where
    baseConfig = nodeConfig . baseConfig

instance HasTrans (DynNode t) t where
    trans = nodeConfig . trans

instance HasSizeConfig (DynNode t) t where
    sizeConfig = nodeConfig . sizeConfig

instance IsNode (DynNode t) where
    toNode (DynNode _ n) = n

instance Eq (DynNode t) where
    (DynNode _ n) == (DynNode _ n') = n == n'

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

instance HasLayerConfig (LayerConfig t) t where
    layerConfig = id

instance HasNodeConfig (LayerConfig t) t where
    nodeConfig = lToNodeConfig

instance HasBaseConfig (LayerConfig t) t where
    baseConfig = nodeConfig . baseConfig

instance HasSizeConfig (LayerConfig t) t where
    sizeConfig = nodeConfig . nToSizeConfig

instance HasTrans (LayerConfig t) t where
    trans = nodeConfig . trans

-- XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double

instance Reflex t => Default (LayerConfig t) where
    def = LayerConfig $ def & anchor .~ constDyn (pure 0.5)
                            & size .~ constDyn (V2 winWidth winHeight)


data DynLayer t = DynLayer (LayerConfig t) Layer

instance HasLayerConfig (DynLayer t) t where
    layerConfig f (DynLayer c l) = (\c' -> DynLayer c' l) <$> f c

instance HasNodeConfig (DynLayer t) t where
    nodeConfig = layerConfig . nodeConfig

instance HasBaseConfig (DynLayer t) t where
    baseConfig = layerConfig . baseConfig

instance HasTrans (DynLayer t) t where
    trans = layerConfig . trans

instance HasSizeConfig (DynLayer t) t where
    sizeConfig = layerConfig . sizeConfig

instance IsNode (DynLayer t) where
    toNode (DynLayer _ l) = toNode l

instance IsLayer (DynLayer t) where
    toLayer (DynLayer _ l) = l

instance Eq (DynLayer t) where
    (DynLayer _ n) == (DynLayer _ n') = n == n'

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

instance HasLayerColorConfig (LayerColorConfig t) t where
    layerColorConfig = id

instance HasColorConfig (LayerColorConfig t) t where
    colorConfig = lcToColorConfig

instance HasLayerConfig (LayerColorConfig t) t where
    layerConfig = lcToLayerConfig

instance HasNodeConfig (LayerColorConfig t) t where
    nodeConfig = layerConfig . nodeConfig

instance HasBaseConfig (LayerColorConfig t) t where
    baseConfig = layerConfig . baseConfig

instance HasTrans (LayerColorConfig t) t where
    trans = layerConfig . trans

instance HasSizeConfig (LayerColorConfig t) t where
    sizeConfig = layerConfig . sizeConfig

instance Reflex t => Default (LayerColorConfig t) where
    def = LayerColorConfig def (ColorConfig $ constDyn black)


data DynLayerColor t = DynLayerColor (LayerColorConfig t) LayerColor

instance HasLayerColorConfig (DynLayerColor t) t where
    layerColorConfig f (DynLayerColor c l) = (\c' -> DynLayerColor c' l) <$> f c

instance HasColorConfig (DynLayerColor t) t where
    colorConfig = layerColorConfig . colorConfig

instance HasLayerConfig (DynLayerColor t) t where
    layerConfig = layerColorConfig . layerConfig

instance HasNodeConfig (DynLayerColor t) t where
    nodeConfig = layerColorConfig . nodeConfig

instance HasBaseConfig (DynLayerColor t) t where
    baseConfig = layerColorConfig . baseConfig

instance HasTrans (DynLayerColor t) t where
    trans = layerColorConfig . trans

instance HasSizeConfig (DynLayerColor t) t where
    sizeConfig = layerColorConfig . sizeConfig

instance IsNode (DynLayerColor t) where
    toNode (DynLayerColor _ l) = toNode l

instance IsLayer (DynLayerColor t) where
    toLayer (DynLayerColor _ l) = toLayer l

instance Eq (DynLayerColor t) where
    (DynLayerColor _ n) == (DynLayerColor _ n') = n == n'

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

instance HasTrans (SpriteConfig t) t where
    trans = baseConfig . trans

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


data DynSprite t = DynSprite (SpriteConfig t) (SizeConfig t) Sprite

instance HasSpriteConfig (DynSprite t) t where
    spriteConfig f (DynSprite c sz s) = (\c' -> DynSprite c' sz s) <$> f c

instance HasBaseConfig (DynSprite t) t where
    baseConfig = spriteConfig . sToBaseConfig

instance HasSizeConfig (DynSprite t) t where
    sizeConfig f (DynSprite c sz s) = (\sz' -> DynSprite c sz' s) <$> f sz

instance HasTrans (DynSprite t) t where
    trans = spriteConfig . trans

instance HasColorConfig (DynSprite t) t where
    colorConfig = spriteConfig . sToColorConfig

instance IsNode (DynSprite t) where
    toNode (DynSprite _ _ s) = toNode s

instance IsSprite (DynSprite t) where
    toSprite (DynSprite _ _ s) = s

instance Eq (DynSprite t) where
    (DynSprite _ _ n) == (DynSprite _ _ n') = n == n'

sprite :: NodeGraph t m => SpriteConfig t -> m (DynSprite t)
sprite conf = do
    s <- createSprite
    appBaseConfig conf s
    appColorConfig conf s
    appDyn (setV setFlippedX setFlippedY s) (conf^.flipped)
    (sizeE, sizeTr) <- newEventWithTriggerRef
    let setSprite [] = return ()
        setSprite name = setSpriteByName s name
        getSz :: MonadIO m => m (V2 Double)
        getSz = V2 <$> getWidth s <*> getHeight s
    runWithActions <- askRunWithActions
    appDyn ?? conf^.spriteName $ \name -> liftIO $ do
      mTr <- readRef sizeTr
      case mTr of
        Just t -> do
          oldSz <- getSz
          setSprite name
          newSz <- getSz
          if newSz /= oldSz then runWithActions [t :=> Identity newSz]
                            else return ()
        _ -> setSprite name
    sz <- getSz
    sizeD <- holdDyn sz sizeE
    askParent >>= flip addChild s
    return $ DynSprite conf (SizeConfig sizeD) s

sprite_ :: NodeGraph t m => SpriteConfig t -> m ()
sprite_ = void . sprite
