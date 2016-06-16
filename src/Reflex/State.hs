{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.State
  (
    DynStateT(DynStateT)
  , runDynStateT
  , execDynStateT
  , askDyn
  , asksDyn
  , zoomDyn
  , modifyDyn
  , modifyDynMaybe
  , pnon
  , pnon'
  ) where


import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Primitive
import Reflex
import Reflex.Extra
import Reflex.Host.Class
import Reflex.Cocos2d.Class
import Data.Maybe
{-
rec uiEvents <- ...
    runDynStateT initial $ do modifyDyn $ (uiReducer :: UIAction -> s -> s) <$> uiEvents
                              viewComponent1
                              -- drillDownToSubState should be some lens
                              zoom1 drillDownToSubState $ viewComponent2
return ()

liftSubTransformer :: (subs -> subs) -> (s -> s)
liftSubTransformer = (someSubState %~)

drillDownToSubState :: s -> subs
drillDownToSubState = (^.someSubState)

-- self contained 'world' that could *actually* be regarded as pluggable component (without holding its own state)
viewComponent1 :: Dynamic t state -> m (Event t (state -> state))
viewComponent1 stateDyn =
    -- create the widgets based on the initial spec / stateDyn
    boxDyns <- forDyn (^.boxes) stateDyn >>= splitsDyn
    boxActions :: [Event t BoxAction] <- forM_ boxDyns $ \bdyn -> do
                      -- create the widget
                      createBox bdyn
    return $ mergeWith (.) $ (fmap (reducer :: BoxAction -> s -> s)) <$> boxActions


----- multiple scenes -----

data AppState = AppState
  { _sharedState :: SharedState
  , _sceneState :: SceneState
  }

data SceneState = Scene1 ... | Scene2 ... | ...

-- is this whole thing a recipe for miserable performance?

sceneEntryComponent :: DynStateT AppState t m ()
sceneEntryComponent = do
  -- sceneChangeDyn <- asks (^.sceneState) -- here maybe take (^.sceneState) but needs to look for false state change caused by sharedState
  -- (modifyDyn =<<) . lift $ do
  --   scenes <- forDyn sceneChangeDyn $ \initialSceneAppState -> do
  --       -- case by case for different sceneState
  --       -- TODO: looks like some lens operation to me!
  --       AppState shared (Scene1 scene1Data) <- initialSceneAppState
  --       (scene2DataEvt, sceneStateDyn) <- flip runDynStateT (shared, scene1Data) $ do viewSceneComponents
  --                                                                                     -- something else...
  --       -- TODO: looks like some lens operation to me!
  --       return $ leftmost [ (\(shared, scene1Data) _ -> AppState shared (Scene1 scene1Data)) <$> (updated sceneStateDyn)
  --                         -- proceed to the next scene
  --                         , (\(shared, scene2Data) _ -> AppState shared (Scene2 scene2Data)) <$> scene2DataEvt
  --                         ]
  --
  --   transformerE <- sceneContainer -< scenes
  --   switchPromptly never transformerE
  --  take advantage of Free here
  sceneContainer -<< (sceneComponent :: FreeT (Event t) (DynStateT s t m) a)


sceneComponent :: FreeT (Event t) (DynStateT AppState t m) ()
sceneComponent = do
  lift $ do
    -- <- ask state
    -- modifyDyn $
-}


---- generalization: DynStateT
---- the input is always the same - like a reader
data DynStateT s t m a = DynStateT
    { _runDynStateT :: !(Dynamic t s -> [Event t (s -> Maybe s)] -> m (a, [Event t (s -> Maybe s)])) }

instance Monad m => Functor (DynStateT s t m) where
    fmap = liftM

instance Monad m => Applicative (DynStateT s t m) where
    (<*>) = ap
    pure = return

instance Monad m => Monad (DynStateT s t m) where
  m >>= k = DynStateT $ \d ts -> do
              (a, ts') <- _runDynStateT m d ts
              _runDynStateT (k a) d ts'
  return a = DynStateT $ \_ ts -> return (a, ts)


{-# INLINE composeMaybe #-}
composeMaybe :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
composeMaybe f g a
  | Just a' <- g a = f a'
  | otherwise      = f a

runDynStateT :: (Reflex t, MonadHold t m, MonadFix m) => DynStateT s t m a -> s -> m (a, Dynamic t s)
runDynStateT ms initial = mdo
    stateDyn <- foldDynMaybe ($) initial $ mergeWith composeMaybe ts
    (a, ts) <- _runDynStateT ms stateDyn []
    return (a, stateDyn)


execDynStateT :: (Reflex t, MonadHold t m, MonadFix m) => DynStateT s t m a -> s -> m (Dynamic t s)
execDynStateT ms initial = snd <$> runDynStateT ms initial

askDyn :: Monad m => DynStateT s t m (Dynamic t s)
askDyn = DynStateT $ \d ts -> return $ (d, ts)

asksDyn :: (Reflex t, MonadHold t m) => (s -> a) -> DynStateT s t m (Dynamic t a)
asksDyn f = DynStateT $ \d ts -> (,ts) <$> mapDyn f d

zoomDyn :: (Reflex t, MonadHold t m, Eq a) => ALens' s a -> DynStateT a t m b -> DynStateT s t m b
zoomDyn = zoomDyn' (/=)

zoomDyn' :: (Reflex t, MonadHold t m) => (a -> a -> Bool) -> ALens' s a -> DynStateT a t m b -> DynStateT s t m b
zoomDyn' p len am = DynStateT $ \ds ts -> do
    let clonedGetter = cloneLens len
        clonedSetter = cloneLens len
    da <- mapDyn (^.clonedGetter) ds
    (b, ta') <- _runDynStateT am (nubDynBy p da) []
    let !lts = clonedSetter <$> mergeWith composeMaybe ta'
    return (b, lts:ts)

modifyDyn :: (Reflex t, Monad m) => Event t (s -> s) -> DynStateT s t m ()
modifyDyn et = let !et' = fmap Just <$> et in DynStateT $ \_ !ts -> return ((), et':ts)

modifyDynMaybe :: Monad m => Event t (s -> Maybe s) -> DynStateT s t m ()
modifyDynMaybe !et = DynStateT $ \_ !ts -> return ((), et:ts)

instance MonadTrans (DynStateT s t) where
    lift m = DynStateT $ \_ ts -> (,ts) <$> m

instance MonadIO m => MonadIO (DynStateT s t m) where
    liftIO = lift . liftIO

-- TODO: implement all the reflex classes
instance MonadSample t m => MonadSample t (DynStateT s t m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (DynStateT s t m) where
    hold v0 = lift . hold v0

instance MonadFix m => MonadFix (DynStateT s t m) where
    mfix f = DynStateT $ \d ts -> mdo
            (a, ts') <- _runDynStateT (f a) d ts
            return (a, ts')

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DynStateT s t m) where
    newEventWithTrigger = lift . newEventWithTrigger
    newFanEventWithTrigger = lift . newFanEventWithTrigger

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (DynStateT s t m) where
    subscribeEvent = lift . subscribeEvent

instance MonadRef m => MonadRef (DynStateT s t m) where
    type Ref (DynStateT s t m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

instance PrimMonad m => PrimMonad (DynStateT s t m) where
    type PrimState (DynStateT s t m) = PrimState m
    primitive = lift . primitive

-- implement NodeGraph instance so that we don't need to keep lifting...
instance NodeGraph t m => NodeGraph t (DynStateT s t m) where
    askParent = lift $ askParent
    askPostBuildEvent = lift $ askPostBuildEvent
    askRunWithActions = lift $ askRunWithActions
    subGraph n (DynStateT f) = DynStateT $ \d ts -> subGraph n (f d ts)
    holdGraph n (DynStateT fa) emb = DynStateT $ \d ts -> do
              ((a, tsZ), erb) <- holdGraph n (fa d []) $ ffor emb $ \(DynStateT fb) -> fb d []
              let et = (mergeWith composeMaybe . snd) <$> erb
                  tz = mergeWith composeMaybe tsZ
              switched <- switchPromptly tz et
              return ((a, fst <$> erb), switched:ts)
    buildEvent e = DynStateT $ \d ts -> mdo
              built <- buildEvent $ flip pushAlways e $ \(DynStateT fa) -> do
                            t <- sample behT
                            return $ fa d [t]
              let et = (mergeWith composeMaybe . snd) <$> built
              behT <- hold never et
              return (fst <$> built, switch behT:ts)
    buildEvent_ = void . buildEvent
    runEventMaybe = lift . runEventMaybe
    runEvent_ = lift . runEvent_
    runEvent = lift . runEvent

---- Helpers

-- | partial non: useful to convert a Prism to something that takes a default
-- e.g., pnon _Just 20 is similar to non 20
-- NOTE: this isn't an Iso, because the conversion from a to s is impossible when a == def
-- that means e.g., Nothing ^. pnon _Just 3 = 3; review (pnon _Just 3) 3 != Nothing
pnon' :: APrism' s a -> a -> Lens' s a
pnon' p def = lens (fromMaybe def . preview (clonePrism p)) (const (review (clonePrism p)))

-- non without the need for Eq (and doesn't reset source to Nothing when default is supplied)
pnon :: a -> Lens' (Maybe a) a
pnon = pnon' _Just
