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
    AccStateT
  , DynStateT
  , EventStateT
  , UniqDynStateT
  , BehaviorStateT
  , watch
  , watches
  , focus
  , refine
  , runAccStateT
  , execAccStateT
  , adjust
  , adjustMaybe
  , pnon
  , pnon'
  ) where


import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Ref
import Reflex
import Reflex.Host.Class
import Reflex.Cocos2d.Class
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
{-
rec uiEvents <- ...
    runAccStateT initial $ do adjust $ (uiReducer :: UIAction -> s -> s) <$> uiEvents
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

sceneEntryComponent :: AccStateT AppState t m ()
sceneEntryComponent = do
  -- sceneChangeDyn <- asks (^.sceneState) -- here maybe take (^.sceneState) but needs to look for false state change caused by sharedState
  -- (adjust =<<) . lift $ do
  --   scenes <- forDyn sceneChangeDyn $ \initialSceneAppState -> do
  --       -- case by case for different sceneState
  --       -- TODO: looks like some lens operation to me!
  --       AppState shared (Scene1 scene1Data) <- initialSceneAppState
  --       (scene2DataEvt, sceneStateDyn) <- flip runAccStateT (shared, scene1Data) $ do viewSceneComponents
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
  sceneContainer -<< (sceneComponent :: FreeT (Event t) (AccStateT s t m) a)


sceneComponent :: FreeT (Event t) (AccStateT AppState t m) ()
sceneComponent = do
  lift $ do
    -- <- ask state
    -- adjust $
-}

---- generalization: AccStateT
---- the input is always the same - like a reader
newtype AccStateT t f s m a = AccStateT
                            { _runAccStateT :: StateT [Event t (s -> Maybe s)] (ReaderT (f s) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

type DynStateT t = AccStateT t (Dynamic t)
type EventStateT t = AccStateT t (Event t)
type UniqDynStateT t = AccStateT t (UniqDynamic t)
type BehaviorStateT t = AccStateT t (Behavior t)

instance MonadTrans (AccStateT t f s) where
    lift = AccStateT . lift . lift

watch :: Monad m => AccStateT t f s m (f s)
watch = AccStateT . lift $ ask

watches :: (Functor f, Monad m) => (s -> a) -> AccStateT t f s m (f a)
watches f = fmap f <$> watch

{-# INLINE composeMaybe #-}
composeMaybe :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
composeMaybe f g a
  | Just a' <- g a = f a'
  | otherwise      = f a

runAccStateT :: (Accumulator t f, MonadHold t m, MonadFix m) => AccStateT t f s m a -> s -> m (a, f s)
runAccStateT ms initial = mdo
    stateDyn <- accumMaybe (&) initial $ mergeWith composeMaybe ts
    (a, ts) <- _runAccStateT' ms stateDyn []
    return (a, stateDyn)

_runAccStateT' :: AccStateT t f s m a -> f s -> [Event t (s -> Maybe s)] -> m (a, [Event t (s -> Maybe s)])
_runAccStateT' ms d ts = flip runReaderT d . flip runStateT ts . _runAccStateT $ ms

execAccStateT :: (Accumulator t f, MonadHold t m, MonadFix m) => AccStateT t f s m a -> s -> m (f s)
execAccStateT ms initial = snd <$> runAccStateT ms initial

focus :: (Reflex t, MonadHold t m, Functor f) => ALens' s a -> AccStateT t f a m b -> AccStateT t f s m b
focus len am = do
    let clonedGetter = cloneLens len
        clonedSetter = cloneLens len
    da <- watches (^.clonedGetter)
    (b, ta') <- lift $ _runAccStateT' am da []
    let !lts = clonedSetter <$> mergeWith composeMaybe ta'
    adjustMaybe lts
    return b

refine :: (f2 a -> f1 a) -> AccStateT t f1 a m b -> AccStateT t f2 a m b
refine f m = AccStateT . StateT $ \ts -> ReaderT $ \d -> _runAccStateT' m (f d) ts

adjust :: (Reflex t, Monad m) => Event t (s -> s) -> AccStateT t f s m ()
adjust et = let !et' = fmap Just <$> et in AccStateT $ modify (et':)

adjustMaybe :: Monad m => Event t (s -> Maybe s) -> AccStateT t f s m ()
adjustMaybe !et = AccStateT $ modify (et:)

instance MonadSample t m => MonadSample t (AccStateT t f s m) where
    sample = lift . sample

instance MonadHold t m => MonadHold t (AccStateT t f s m) where
    hold v0 = lift . hold v0
    holdDyn v0 = lift . holdDyn v0
    holdIncremental v0 = lift . holdIncremental v0

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (AccStateT t f s m) where
    newEventWithTrigger = lift . newEventWithTrigger
    newFanEventWithTrigger = lift . newFanEventWithTrigger

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (AccStateT t f s m) where
    subscribeEvent = lift . subscribeEvent

instance MonadRef m => MonadRef (AccStateT t f s m) where
    type Ref (AccStateT t f s m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r


-- implement NodeGraph instance so that we don't need to keep lifting...
instance NodeGraph t m => NodeGraph t (AccStateT t f s m) where
    askParent = lift $ askParent
    askPostBuildEvent = lift $ askPostBuildEvent
    askRunWithActions = lift $ askRunWithActions
    subGraph n m = AccStateT . StateT $ \ts -> ReaderT $ \d -> subGraph n (_runAccStateT' m d ts)
    holdGraph n ma emb = do
      d <- watch
      ((a, tsZ), erb) <- lift $ holdGraph n (_runAccStateT' ma d []) $ ffor emb $ \mb -> _runAccStateT' mb d []
      let et = (mergeWith composeMaybe . snd) <$> erb
          tz = mergeWith composeMaybe tsZ
      switchPromptly tz et >>= adjustMaybe
      return (a, fst <$> erb)
    buildEvent em = mdo
      d <- watch
      built <- lift . buildEvent $ flip pushAlways em $ \m -> do
                    t <- sample behT
                    return $ _runAccStateT' m d [t]
      let et = (mergeWith composeMaybe . snd) <$> built
      behT :: Behavior t (Event t (s -> Maybe s)) <- hold (never :: Event t (s -> Maybe s)) et
      adjustMaybe $ switch behT
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
