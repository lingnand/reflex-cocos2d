{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Extra
    ( dropWhileE
    , breakE
    , accumEWith
    , accumE
    , postponeCurrent
    , postpone
    , dynMaybe
    , modulate
    , stack
    , distribute
    -- * Free stuff
    , runWithReplaceFree
    , waitEvent
    , waitEvent'
    , waitEvent_
    , waitDynMaybe
    , waitDynMaybe'
    , waitDynMaybe_
    , switchFree
    , switchFreeT
    -- * Rand stuff
    , liftRandE
    )
  where

import Data.Tuple (swap)
import Data.Maybe
import Data.Semigroup
import Reflex
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Random
import Control.Applicative
import Control.Lens

-- | Only push events when a condition is no longer true
dropWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
dropWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (not $ f a) >> return e
    switchPromptly never =<< headE e'

-- | Split an Event into two parts on a condition
spanE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a, Event t a)
spanE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachPromptlyDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    gateE' <- headE gateE
    bef <- switch <$> hold e' (never <$ gateE')
    aft <- switchPromptly never (e <$ gateE')
    return (bef, aft)

breakE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a, Event t a)
breakE = spanE . fmap not

-- | Instead of switching over an Event t (Event t a), we return an Event
-- that merges all these Events together into a single Event t a
accumEWith :: (Reflex t, MonadFix m, MonadHold t m)
           => (a -> a -> a) -> Event t a -> Event t (Event t a) -> m (Event t a)
accumEWith f ze ee = switch <$> accum onE ze ee
  where onE old e = mergeWith f [ old, e ]

accumE :: (Reflex t, MonadFix m, MonadHold t m, Semigroup a)
       => Event t a -> Event t (Event t a) -> m (Event t a)
accumE ze ee = switch <$> accum (<>) ze ee

-- | Convert Dynamic to an Event that carries the first value in postBuild
postponeCurrent :: (Reflex t, PostBuild t m) => Dynamic t a -> m (Event t a)
postponeCurrent d = do
    pe <- getPostBuild
    return $ leftmost [ pushAlways (const $ sample (current d)) pe
                      , updated d ]

postpone :: PostBuild t m => a -> m (Event t a)
postpone v = fmap (const v) <$> getPostBuild

-- | Convert an Event into a Dynamic of Maybe
dynMaybe :: (Reflex t, MonadHold t m)
         => Event t a -> m (Dynamic t (Maybe a))
dynMaybe e = holdDyn Nothing $ Just <$> e

modulate :: (Reflex t, MonadHold t m, MonadFix m, Num a, Ord a) => a -> Event t a -> m (Event t a)
modulate limit = mapAccumMaybe_ f (0, limit)
    where
      f (acc, l) d = let sum = acc + d in
        if sum > l then (Just (0  , limit-(sum-l)) , Just sum)
                   else (Just (sum, l            ) , Nothing )

-- unfoldr :: (b -> Maybe (b, a)) -> b -> [a]
--
-- unfoldE :: Event t (b -> Maybe (b, a)) -> b -> Dynamic t [a]
--
-- commute :: Event t (Maybe a -> Maybe b) -> Event t (Maybe b -> Maybe a)
--         -> ([a], [b])
--         -> m (Dynamic t ([a], [b]))

-- | Simple stack that responds to Events
stack :: (Reflex t, MonadHold t m, MonadFix m)
      => Event t (Maybe a -> b) -- ^ the request Event for taking items out / popping
      -> [a] -- ^ the initial state
      -> Event t a -- ^ the input states
      -> m (Dynamic t [a], Event t b) -- ^ (stack Dynamic, the output Event)
stack reqs z input = do
    rec let e = mergeWith (\f1 f2 a -> let (a', e1)  = f1 a
                                           (a'', e2) = f2 (fromMaybe a a')
                                       in (a'' <|> a', e2 <|> e1)
                          )
                [ (\i a -> (Just (i:a), Nothing)) <$> input
                , ffor reqs $ \f -> \case
                    [] -> (Nothing, Just $ f Nothing) -- if there is nothing to take out, we get Nothing
                    (a:as) -> (Just as, Just . f $ Just a)
                ]
            e' = push ?? e $ \f -> do
                      a <- sample b'
                      return . Just $ f a
            se' = fmapMaybe fst e'
        b' <- hold z se'
    return (unsafeDynamic b' se', fmapMaybe snd e')

--          workerFunc                          input           dynamically resized list of workers where we get the output
-- (allocatedInput -> m (Event t output)) -> Event t a -> Dynamic t [Event t output]

-- | Distribute a upstream 'task' Event into a list of 'worker' Events, such that
-- * each input task is sent to only one of the workers
-- * a worker receiving an input task won't receive another task until it sends a 'done' Event back
distribute :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t a -- ^ input task Event
           -> Int -- ^ number of workers (TODO: this is needed because we can't read "the list of Done Events" directly)
           -> [Event t b] -- ^ the list of Done Events
           -- ( Dynamic for the indices of idle workers, output Events, failed allocation Events )
           -> m (Dynamic t [Int], [Event t a], Event t a)
distribute tasks n workerDones = do
    -- TODO: leftmost would cut some Done Events away
    let ids = [1..n]
        donesE = leftmost $ zipWith (<$) ids workerDones
    (dSt, out) <- stack ((,) <$> tasks) ids donesE
    let failed = fforMaybe out $ \case
                    (t, Nothing) -> Just t
                    _ -> Nothing
        out' = fforMaybe out $ \case
                    (t, Just id) -> Just (t, id)
                    _ -> Nothing
        dist = ffor ids $ \id -> fforMaybe out' $ \(t, id') -> guard (id' == id) >> return t
    return (dSt, dist, failed)

-- Free related

-- | Greedy Free
runWithReplaceFree ::
  forall t m a.
  (MonadFix m , PostBuild t m , MonadHold t m, MonadAdjust t m)
  => FreeT (Event t) m a -> m (Event t a)
runWithReplaceFree ft = do
    rec (result0, newResult) <- runWithReplace (runFreeT ft) (traceEventWith (const "FreeT computation fired") (runFreeT <$> newFs))
        let startE = case result0 of
              Pure _ -> never :: Event t (FreeT (Event t) m a)
              Free e -> e
        newFs <- switchPromptly startE $ (traceEventWith (const "FreeT result arrived") $ fmapMaybe previewFree newResult)
    case result0 of
      Pure a -> postpone a
      _ -> return $ fmapMaybe previewPure newResult

-- ** Operations to be used in seqHoldFree
-- | Wait for the first occurrence
waitEvent :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m a
waitEvent = liftF

-- | Wait for the first occurrence and include the future occurrences in return
waitEvent' :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m (a, Event t a)
waitEvent' e = (,e) <$> liftF e

waitEvent_ :: (Reflex t, Monad m) => Event t a -> FreeT (Event t) m ()
waitEvent_ = void . waitEvent

-- | Wait for the Dynamic to turn from Nothing to Just
waitDynMaybe :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m a
waitDynMaybe dyn = lift (sample $ current dyn) >>= \case
    Just a -> return a
    _ -> waitEvent $ fmapMaybe id (updated dyn)

-- | Wait for the first Just value, and include the future values in return
waitDynMaybe' :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m (a, Event t a)
waitDynMaybe' dyn = (,fmapMaybe id $ updated dyn) <$> waitDynMaybe dyn

waitDynMaybe_ :: (Reflex t, MonadSample t m) => Dynamic t (Maybe a) -> FreeT (Event t) m ()
waitDynMaybe_ = void . waitDynMaybe

previewFree :: FreeF f a b -> Maybe (f b)
previewFree (Free fb) = Just fb
previewFree _ = Nothing

previewPure :: FreeF f a b -> Maybe a
previewPure (Pure a) = Just a
previewPure _ = Nothing


switchFreeT' ::
  (Reflex t, MonadHold t m)
  => (forall x. m' x -> m x)
  -> (forall x. m' x -> PushM t x)
  -> FreeT (Event t) m' a -> m (FreeF (Event t) a a)
switchFreeT' hoistM hoistPush ft = hoistM (runFreeT ft) >>= \case
    Pure a -> return $ Pure a
    Free e -> Free <$> switchPromptly never flattened
      where flattened = flip pushAlways e $ switchFreeT' hoistPush hoistPush >=> \case
                            Pure a -> return $ a <$ e
                            Free ie -> return ie

-- | Merge a deeply nested Event into a single Event
switchFree ::
  (Reflex t, MonadHold t m, PostBuild t m)
  => Free (Event t) a -> m (Event t a)
switchFree f = do
    let hoist (Identity x) = return x
    switchFreeT' hoist hoist f >>= \case
      Pure a -> postpone a
      Free e -> return e

switchFreeT ::
  (Reflex t, PostBuild t m, MonadHold t m)
  => FreeT (Event t) (PushM t) a -> m (Event t a)
switchFreeT f = do
    e <- getPostBuild
    let e' = flip pushAlways e $ \_ -> switchFreeT' id id f >>= \case
          Pure a -> return $ a <$ e
          Free e -> return e
    switchPromptly never e'

-- Random helpers

liftRandE :: (Reflex t, MonadFix m, MonadHold t m, RandomGen g)
             => Event t (Rand g a) -> RandT g m (Event t a)
liftRandE e = liftRandT $ \g ->
  let (g1, g2) = split g in (, g2) <$> mapAccum_ (\g m -> swap $ runRand m g) g1 e

-- | Fold a pure transformer monad over an event and get the resultant
-- values back
-- foldT :: forall t trans m a .
--          ( Reflex t, MonadFix m, MonadHold t m
--          , MonadTransControl trans, Monad (trans Identity) )
--       => (forall a. trans Identity a -> a) -> Event t (trans Identity a) -> m (Event t a)
-- foldT unT evt = do
--   let foldF :: Maybe (StT trans ()) -> trans Identity a -> (Maybe (StT trans ()), a)
--       foldF maybeLastState newM = unT $ do
--         mapM_ (restoreT .  return :: StT trans () -> trans Identity ()) maybeLastState
--         flip (,) <$> newM <*> (Just <$> captureT)
--   mapAccum_ foldF Nothing evt


-- instance {-# OVERLAPPABLE #-} (MonadTransControl trans, PerformEvent t m)
--   => PerformEvent t (trans m) where
--     type Performable m = trans (Performable m)
--     performEvent_ evt = do
