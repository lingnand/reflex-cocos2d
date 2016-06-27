{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    MonadDynReader(..)
  , asksDyn
  , DynReaderT
  , runDynReaderT
  , localDyn
  , DynStateT
  , liftDynReader
  , runDynStateT
  , execDynStateT
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
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

class Monad m => MonadDynReader r t m | m -> r t where
    askDyn :: m (Dynamic t r)

asksDyn :: (Reflex t, MonadDynReader r t m, MonadHold t m) => (r -> a) -> m (Dynamic t a)
asksDyn f = askDyn >>= mapDyn f

newtype DynReaderT r t m a = DynReaderT { _runDynReaderT :: (ReaderT (Dynamic t r) m a) }
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadFix, MonadIO
           , MonadSample t, MonadHold t, MonadReflexCreateTrigger t, MonadSubscribeEvent t )

instance MonadRef m => MonadRef (DynReaderT r t m) where
    type Ref (DynReaderT r t m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

instance PrimMonad m => PrimMonad (DynReaderT r t m) where
    type PrimState (DynReaderT r t m) = PrimState m
    primitive = lift . primitive

instance Monad m => MonadDynReader r t (DynReaderT r t m) where
    askDyn = DynReaderT ask

instance NodeGraph t m => NodeGraph t (DynReaderT r t m) where
    askParent = lift askParent
    askPostBuildEvent = lift askPostBuildEvent
    askRunWithActions = lift askRunWithActions
    subGraph n rm = askDyn >>= lift . subGraph n . runDynReaderT rm
    holdGraph n m emb = do
      d <- askDyn
      lift $ holdGraph n (runDynReaderT m d) (flip runDynReaderT d <$> emb)
    buildEvent e = askDyn >>= lift . buildEvent . (<$> e) . flip runDynReaderT
    buildEvent_ = void . buildEvent
    runEventMaybe = lift . runEventMaybe
    runEvent_ = lift . runEvent_
    runEvent = lift . runEvent

runDynReaderT :: DynReaderT r t m a -> Dynamic t r -> m a
runDynReaderT = runReaderT . _runDynReaderT

localDyn :: (Reflex t, MonadHold t m) => (r -> r') -> DynReaderT r' t m a -> DynReaderT r t m a
localDyn f a = DynReaderT . ReaderT $ \d -> mapDyn f d >>= runDynReaderT a

---- generalization: DynStateT
---- the input is always the same - like a reader
newtype DynStateT s t m a = DynStateT
                          { _runDynStateT :: StateT [Event t (s -> Maybe s)] (DynReaderT s t m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadTrans (DynStateT s t) where
    lift = DynStateT . lift . lift

instance Monad m => MonadDynReader s t (DynStateT s t m) where
    askDyn = DynStateT . lift $ askDyn

{-# INLINE composeMaybe #-}
composeMaybe :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
composeMaybe f g a
  | Just a' <- g a = f a'
  | otherwise      = f a

runDynStateT :: (Reflex t, MonadHold t m, MonadFix m) => DynStateT s t m a -> s -> m (a, Dynamic t s)
runDynStateT ms initial = mdo
    stateDyn <- foldDynMaybe ($) initial $ mergeWith composeMaybe ts
    (a, ts) <- _runDynStateT' ms stateDyn []
    return (a, stateDyn)

_runDynStateT' :: DynStateT s t m a -> Dynamic t s -> [Event t (s -> Maybe s) ] -> m (a, [Event t (s -> Maybe s)])
_runDynStateT' ms d ts = flip runDynReaderT d . flip runStateT ts . _runDynStateT $ ms

liftDynReader :: Monad m => DynReaderT s t m a -> DynStateT s t m a
liftDynReader mr = DynStateT $ lift mr

execDynStateT :: (Reflex t, MonadHold t m, MonadFix m) => DynStateT s t m a -> s -> m (Dynamic t s)
execDynStateT ms initial = snd <$> runDynStateT ms initial

zoomDyn :: (Reflex t, MonadHold t m, Eq a) => ALens' s a -> DynStateT a t m b -> DynStateT s t m b
zoomDyn = zoomDyn' (/=)

zoomDyn' :: (Reflex t, MonadHold t m) => (a -> a -> Bool) -> ALens' s a -> DynStateT a t m b -> DynStateT s t m b
zoomDyn' p len am = do
    let clonedGetter = cloneLens len
        clonedSetter = cloneLens len
    da <- asksDyn (^.clonedGetter)
    (b, ta') <- lift $ flip runDynReaderT (nubDynBy p da) . flip runStateT [] . _runDynStateT $ am
    let !lts = clonedSetter <$> mergeWith composeMaybe ta'
    modifyDynMaybe lts
    return b

modifyDyn :: (Reflex t, Monad m) => Event t (s -> s) -> DynStateT s t m ()
modifyDyn et = let !et' = fmap Just <$> et in DynStateT $ modify (et':)

modifyDynMaybe :: Monad m => Event t (s -> Maybe s) -> DynStateT s t m ()
modifyDynMaybe !et = DynStateT $ modify (et:)

instance MonadSample t m => MonadSample t (DynStateT s t m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (DynStateT s t m) where
    hold v0 = lift . hold v0

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
    subGraph n m = DynStateT . StateT $ \ts -> subGraph n (flip runStateT ts $ _runDynStateT m)
    holdGraph n ma emb = do
      d <- askDyn
      ((a, tsZ), erb) <- lift $ holdGraph n (_runDynStateT' ma d []) $ ffor emb $ \mb -> _runDynStateT' mb d []
      let et = (mergeWith composeMaybe . snd) <$> erb
          tz = mergeWith composeMaybe tsZ
      switchPromptly tz et >>= modifyDynMaybe
      return (a, fst <$> erb)
    buildEvent em = mdo
      d <- askDyn
      built <- lift . buildEvent $ flip pushAlways em $ \m -> do
                    t <- sample behT
                    return $ _runDynStateT' m d [t]
      let et = (mergeWith composeMaybe . snd) <$> built
      behT <- hold never et
      modifyDynMaybe (switch behT)
      return $ fst <$> built
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
