{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Cocos2d.Node
    (
      NodeConfig(NodeConfig)
    , LayerConfig(LayerConfig)
    , LayerColorConfig(LayerColorConfig)
    , HasNodeConfig(..)
    , HasLayerConfig(..)
    , HasLayerColorConfig(..)
    , WithConf
    , DynNode
    , DynLayer
    , node
    , node_
    , nodeHold
    , nodeView
    , layer
    , layer_
    , layerHold
    , layerView
    , layerColor
    , layerColor_
    , layerColorHold
    , layerColorView
    ) where

import Data.Dependent.Sum (DSum (..))
import Data.Colour
import Data.Colour.Names
import Data.Default
import Control.Monad
import Control.Monad.Ref
import Control.Monad.IO.Class
import Control.Lens
import Linear
import Reflex
import Reflex.Host.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import Reflex.Cocos2d.Class

data NodeConfig t = NodeConfig
    { _position :: Dynamic t (V2 Double)
    , _size :: Dynamic t (V2 Double) -- ^ Size as a vector (width: x, height: y)
    , _anchor :: Dynamic t (V2 Double)
    , _skew :: Dynamic t (V2 Double)
    , _zIndex :: Dynamic t Int
    , _rotation :: Dynamic t (V2 Double)
    , _scale :: Dynamic t (V2 Double)
    , _visible :: Dynamic t Bool
    , _color :: Dynamic t (Colour Double)
    , _opacity :: Dynamic t Double -- ^ 0.0 - 1.0
    , _cascadeColor :: Bool
    , _cascadeOpacity :: Bool
    }
makeClassy ''NodeConfig

newtype LayerConfig t = LayerConfig (NodeConfig t)

instance HasNodeConfig (LayerConfig t) t where
    nodeConfig f (LayerConfig c) = (\c' -> LayerConfig c') <$> f c

class HasNodeConfig c t => HasLayerConfig c t where
    layerConfig :: Lens' c (LayerConfig t)

instance HasLayerConfig (LayerConfig t) t where
    layerConfig = id

newtype LayerColorConfig t = LayerColorConfig (LayerConfig t)

instance HasNodeConfig (LayerColorConfig t) t where
    nodeConfig f (LayerColorConfig (LayerConfig c)) = (\c' -> LayerColorConfig $ LayerConfig c') <$> f c

instance HasLayerConfig (LayerColorConfig t) t where
    layerConfig f (LayerColorConfig c) = (\c' -> LayerColorConfig c') <$> f c

class HasLayerConfig c t => HasLayerColorConfig c t where
    layerColorConfig :: Lens' c (LayerColorConfig t)

instance HasLayerColorConfig (LayerColorConfig t) t where
    layerColorConfig = id

instance Reflex t => Default (NodeConfig t) where
    def = NodeConfig { _position = constDyn zero
                     , _size = constDyn zero
                     , _anchor = constDyn zero
                     , _skew = constDyn zero
                     , _zIndex = constDyn 0
                     , _rotation = constDyn zero
                     , _scale = constDyn $ pure 1.0
                     , _visible = constDyn True
                     , _color = constDyn white
                     , _opacity = constDyn 1.0
                     , _cascadeColor = False
                     , _cascadeOpacity = False
                     }

instance Reflex t => Default (LayerConfig t) where
    def = LayerConfig $ def & anchor .~ constDyn (pure 0.5)
                            & size .~ constDyn (V2 winWidth winHeight)

instance Reflex t => Default (LayerColorConfig t) where
    def = LayerColorConfig $ def & color .~ constDyn black

-- | Used to interact with ghcjs-cocos2d as well as keeping
-- a reference of the relevant config
data WithConf c n = WithConf c n

-- | Convenience constraints
type DynNode n t = (IsNode n, HasNodeConfig n t)
type DynLayer l t = (IsLayer l, HasLayerConfig l t)

instance IsNode n => IsNode (WithConf c n) where
    toNode (WithConf _ n) = toNode n

instance IsLayer l => IsLayer (WithConf c l) where
    toLayer (WithConf _ l) = toLayer l

instance HasNodeConfig c t => HasNodeConfig (WithConf c e) t where
    nodeConfig f (WithConf c n) = (\nc -> WithConf (c & nodeConfig.~nc) n) <$> f (c^.nodeConfig)

instance HasLayerConfig c t => HasLayerConfig (WithConf c e) t where
    layerConfig f (WithConf c n) = (\lc -> WithConf (c & layerConfig.~lc) n) <$> f (c^.layerConfig)

instance HasLayerColorConfig c t => HasLayerColorConfig (WithConf c e) t where
    layerColorConfig f (WithConf c n) = (\lc -> WithConf (c & layerColorConfig.~lc) n) <$> f (c^.layerColorConfig)

node :: NodeGraph t m => NodeConfig t -> m a -> m (WithConf (NodeConfig t) Node, a)
node conf child = createNode >>=
    liftM2 (,) <$> return . WithConf conf <*> integrate conf child

node_ :: NodeGraph t m => NodeConfig t -> m (WithConf (NodeConfig t) Node)
node_ conf = fst <$> node conf (return ())

nodeHold :: NodeGraph t m => NodeConfig t -> m a -> Event t (m a) -> m (WithConf (NodeConfig t) Node, Dynamic t a)
nodeHold conf child0 newChild = createNode >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateHold conf child0 newChild

nodeView :: NodeGraph t m => NodeConfig t -> Dynamic t (m a) -> m (WithConf (NodeConfig t) Node, Event t a)
nodeView conf child = createNode >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateView conf child

layer :: NodeGraph t m => LayerConfig t -> m a -> m (WithConf (LayerConfig t) Layer, a)
layer conf child = createLayer >>=
    liftM2 (,) <$> return . WithConf conf <*> integrate conf child

layer_ :: NodeGraph t m => LayerConfig t -> m (WithConf (LayerConfig t) Layer)
layer_ conf = fst <$> layer conf (return ())

layerHold :: NodeGraph t m => LayerConfig t -> m a -> Event t (m a) -> m (WithConf (LayerConfig t) Layer, Dynamic t a)
layerHold conf child0 newChild = createLayer >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateHold conf child0 newChild

layerView :: NodeGraph t m => LayerConfig t -> Dynamic t (m a) -> m (WithConf (LayerConfig t) Layer, Event t a)
layerView conf child = createLayer >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateView conf child

layerColor :: NodeGraph t m => LayerColorConfig t -> m a -> m (WithConf (LayerColorConfig t) LayerColor, a)
layerColor conf child = createLayerColor >>=
    liftM2 (,) <$> return . WithConf conf <*> integrate conf child

layerColor_ :: NodeGraph t m => LayerColorConfig t -> m (WithConf (LayerColorConfig t) LayerColor)
layerColor_ conf = fst <$> layerColor conf (return ())

layerColorHold :: NodeGraph t m => LayerColorConfig t -> m a -> Event t (m a) -> m (WithConf (LayerColorConfig t) LayerColor, Dynamic t a)
layerColorHold conf child0 newChild = createLayerColor >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateHold conf child0 newChild

layerColorView :: NodeGraph t m => LayerColorConfig t -> Dynamic t (m a) -> m (WithConf (LayerColorConfig t) LayerColor, Event t a)
layerColorView conf child = createLayerColor >>=
    liftM2 (,) <$> return . WithConf conf <*> integrateView conf child

applyNodeConfig :: (IsNode n, NodeGraph t m, HasNodeConfig c t) => n -> c -> m ()
applyNodeConfig n c | (NodeConfig pos size anchor skew zIndex rotation scale
                      visible color opacity cascadeColor cascadeOpacity) <- c^.nodeConfig = do
    -- schedule post all values that requires lazy input
    let app setter dyn = do
            schedulePostBuild $ setter n =<< sample (current dyn)
            forH_ (updated dyn) $ setter n
        vset xset yset n (V2 x y) = xset n x >> yset n y
    app (vset setX setY) pos
    app (vset setWidth setHeight) size
    app (vset setAnchorX setAnchorY) anchor
    app (vset setSkewX setSkewY) skew
    app setZIndex zIndex
    app (vset setRotationX setRotationY) rotation
    app (vset setScaleX setScaleY) scale
    app setVisible visible
    app setColor color
    app setOpacity opacity
    setCascadeColor n cascadeColor
    setCascadeOpacity n cascadeOpacity

integrate :: (NodeGraph t m, IsNode n, HasNodeConfig c t) => c -> m a -> n -> m a
integrate conf child n = integrate' conf n >> subGraph (toNode n) child

integrateHold :: (NodeGraph t m, IsNode n, HasNodeConfig c t) => c -> m a -> Event t (m a) -> n -> m (Dynamic t a)
integrateHold conf child0 newChild n = do
    integrate' conf n
    (result0, newResult) <- graphHold (toNode n) child0 newChild
    holdDyn result0 newResult

integrateView :: (NodeGraph t m, IsNode n, HasNodeConfig c t) => c -> Dynamic t (m a) -> n -> m (Event t a)
integrateView conf child n = do
    integrate' conf n
    (e, trigger) <- newEventWithTriggerRef
    runWithActions <- askRunWithActions
    schedulePostBuild . liftIO $ readRef trigger >>= mapM_ (\t -> runWithActions [t :=> ()])
    let newChild = leftmost [updated child, tag (current child) e]
    snd <$> graphHold (toNode n) (return ()) newChild

integrate' :: (NodeGraph t m, IsNode n, HasNodeConfig c t) => c -> n -> m ()
integrate' conf n = do
    applyNodeConfig n conf
    p <- askParent
    addChild p n

graphHold :: NodeGraph t m => Node -> m a -> Event t (m b) -> m (a, Event t b)
graphHold p child0 newChild = do
    (result0, voidAction0) <- subGraphWithVoidActions p child0
    (newChildBuilt, newChildBuiltTriggerRef) <- newEventWithTriggerRef
    voidAction <- hold voidAction0 $ fmap snd newChildBuilt
    performEvent_ $ switch voidAction
    runGraph <- askRunGraph
    runWithActions <- askRunWithActions
    forH_ newChild $ \g -> do
        removeAllChildren p
        (r, postBuild, vas) <- runGraph p g
        mt <- readRef newChildBuiltTriggerRef
        liftIO $ forM_ mt $ \t -> runWithActions [t :=> (r, vas)]
        postBuild
    return (result0, fmap fst newChildBuilt)

-- XXX: HACK - obtain the window size as a constant (there should be a better way?)
foreign import javascript unsafe "cc.winSize.width" winWidth :: Double
foreign import javascript unsafe "cc.winSize.height" winHeight :: Double
