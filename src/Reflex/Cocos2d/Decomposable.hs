{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Reflex.Cocos2d.Decomposable
  (
    Decomposable(..)
  ) where

import Reflex
import Data.Pool
import Control.Monad
import Control.Monad.Fix
import qualified Data.List   as L
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Array  as A

class Decomposable container where
  type Component container :: *
  -- | Decompose a Dynamic data structure into components that are changing over time
  --   The return value is a Dynamic over a list, which records all the *new*
  --   components that appear over time.
  --   The element inside the list is Dynamic t (Maybe (Component container)),
  --   where Maybe signifies the possible disappearance of the component at different time points
  decomposeDyn
    :: ( Eq (Component container)
       , Reflex t, MonadHold t m, MonadFix m )
    => Dynamic t container -> m (Dynamic t [Dynamic t (Maybe (Component container))])

instance Decomposable (IM.IntMap v) where
  type Component (IM.IntMap v) = (Int, v)
  decomposeDyn = decomposeIntMapDyn (fmap . (,))

instance Decomposable [v] where
  type Component [v] = v
  -- probably need to convert into a IntMap first
  decomposeDyn dList = decomposeIntMapDyn (flip const) dMap
    where dMap = IM.fromList . zip [0..] <$> dList

instance Decomposable (Pool a) where
  type Component (Pool a) = a
  decomposeDyn = decomposeIntMapDyn (\_ v -> join v) . (slots <$>)

instance Ord k => Decomposable (M.Map k v) where
  type Component (M.Map k v) = (k, v)
  decomposeDyn = decomposeMapDyn (fmap . (,))

instance A.Ix i => Decomposable (A.Array i e) where
  type Component (A.Array i e) = (i, e)
  decomposeDyn dArr = do
    let updatef a oldIndices | diff <- A.indices a L.\\ oldIndices
                          , not (L.null diff) = Just diff
                          | otherwise = Nothing
        safeIndex i arr | A.inRange (A.bounds arr) i = Just $ arr A.! i
                        | otherwise = Nothing
    dIndices <- scanDynMaybe A.indices updatef dArr
    return $ map (\i -> uniqDyn $ fmap (i,) . safeIndex i <$> dArr) <$> dIndices

decomposeIntMapDyn
  :: ( Eq x
     , Reflex t, MonadHold t m, MonadFix m )
  => (Int -> Maybe v -> x)
  -> Dynamic t (IM.IntMap v)
  -> m (Dynamic t [Dynamic t x])
decomposeIntMapDyn trans dMap = do
  let updatef m oldKeys | diff <- IM.keysSet m `IS.difference` oldKeys
                        , not (IS.null diff) = Just diff
                        | otherwise = Nothing
  dKeys <- scanDynMaybe IM.keysSet updatef dMap
  return $ map (\k -> uniqDyn $ trans k . IM.lookup k <$> dMap) . IS.elems <$> dKeys

-- similar to IntMap but with Map instead
decomposeMapDyn
  :: ( Eq x, Ord k
     , Reflex t, MonadHold t m, MonadFix m )
  => (k -> Maybe v -> x)
  -> Dynamic t (M.Map k v)
  -> m (Dynamic t [Dynamic t x])
decomposeMapDyn trans dMap = do
  let updatef m oldKeys | diff <- M.keysSet m `S.difference` oldKeys
                        , not (S.null diff) = Just diff
                        | otherwise = Nothing
  dKeys <- scanDynMaybe M.keysSet updatef dMap
  return $ map (\k -> uniqDyn $ trans k . M.lookup k <$> dMap) . S.elems <$> dKeys
