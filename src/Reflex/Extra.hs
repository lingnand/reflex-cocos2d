{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Extra
    ( takeWhileE
    , dropWhileE
    , breakE
    , dynMaybe
    , modulate
    , stack
    , distribute
    )
  where

import Data.Maybe
import Reflex
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Control.Lens

-- | Efficiently cut off a stream of events at a point
takeWhileE :: (Reflex t, MonadHold t m, MonadFix m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
    let gateE = fforMaybe e $ \a -> guard (not $ f a) >> return False
    gateDyn <- holdDyn True gateE
    let e' = attachPromptlyDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    fmap switch $ hold e' =<< headE (never <$ gateE)

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
    let e' = attachPromptlyDynWithMaybe (\g a -> guard g >> return a) gateDyn e
    gateE' <- headE gateE
    bef <- switch <$> hold e' (never <$ gateE')
    aft <- switchPromptly never (e <$ gateE')
    return (bef, aft)

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
