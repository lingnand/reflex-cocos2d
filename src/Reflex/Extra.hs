{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Extra (
    nubDynBy
  , mapMDyn
  , forMDyn
  , mapAccumMaybe
  , mapAccum
  , mapAccumMaybe1
  , takeWhileE
  , dropWhileE
  , breakE
  , switchF'
  , stack
  , distribute
  ) where

import Reflex
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Free
import Data.Maybe
import Control.Applicative
import Control.Lens

nubDynBy :: Reflex t => (a -> a -> Bool) -> Dynamic t a -> Dynamic t a
nubDynBy p d =
    let e' = attachWithMaybe (\x x' -> guard (p x x') >> return x') (current d) (updated d)
    in unsafeDynamic (current d) e' --TODO: Avoid invalidating the outgoing Behavior

mapMDyn :: forall t m a b. (Reflex t, MonadSample t m, MonadHold t m, MonadFix m)
        => (forall m'. (MonadSample t m', MonadHold t m', MonadFix m') => a -> m' b)
        -> Dynamic t a
        -> m (Dynamic t b)
mapMDyn f dyn = do
  z <- f =<< sample (current dyn)
  holdDyn z $ pushAlways f (updated dyn)

forMDyn :: forall t m a b. (Reflex t, MonadSample t m, MonadHold t m, MonadFix m)
        =>  Dynamic t a
        -> (forall m'. (MonadSample t m', MonadHold t m', MonadFix m') => a -> m' b)
        -> m (Dynamic t b)
forMDyn dyn f = do
  z <- f =<< sample (current dyn)
  holdDyn z $ pushAlways f (updated dyn)

mapAccumMaybe :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe f z e = do
    e' <- foldDyn (\b (a, _) -> f a b) (z, Nothing) e
    return . fmapMaybe snd $ updated e'

mapAccum :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum f = mapAccumMaybe $ \a b -> let (a', c) = f a b in (a', Just c)

-- | A slight variation of mapAccumMaybe where we use the first event occurrence itself as the
-- starting accumulator
-- This is useful to, e.g., do concat based ops
-- bothOccurred e1 e2 = mapAccumMaybe1 (\a b -> let a' = a <> b in (a', guard $ isThese a'))  (align e1 e2)
mapAccumMaybe1 :: (Reflex t, MonadHold t m, MonadFix m) => (a -> a -> (a, Maybe c)) -> Event t a -> m (Event t c)
mapAccumMaybe1 f e = mapAccumMaybe f' Nothing e
  where f' Nothing a = (Just a, Nothing)
        f' (Just a) a' =  let (a'', emitted) = f a a' in (Just a'', emitted)

switchF' :: (Reflex t, MonadHold t m) => Free (Event t) a -> m (FreeF (Event t) a a)
switchF' ft = case runFree ft of
    Pure a -> return $ Pure a
    Free e -> Free <$> switchPromptly never flattened
      where flattened = flip pushAlways e $ switchF' >=> \case
                            Pure a -> return $ a <$ e
                            Free ie -> return ie

-- | Efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    return . switch =<< hold e' =<< headE (never <$ gateE)

-- | Efficiently cut off a stream of events at a point
dropWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
dropWhileE f e = do
    let e' = fforMaybe e $ \a -> guard (not $ f a) >> return e
    switchPromptly never =<< headE e'

-- | Split an Event into two parts on a condition
breakE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a, Event t a)
breakE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    gateE' <- headE gateE
    bef <- switch <$> hold e' (never <$ gateE')
    aft <- switchPromptly never (e <$ gateE')
    return (bef, aft)

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
