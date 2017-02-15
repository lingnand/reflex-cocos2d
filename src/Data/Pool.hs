{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- A simple structure for a pool of resources.
-- Each managed resource has an integer index that identifies the 'slot' for the given resource;
-- when the resource is idle, this slot holds a Nothing; when it's currently used it holds the
-- content of the resource
module Data.Pool
  (
    Pool
  -- * Accessors
  , busy
  , idles
  , slots
  -- * Query
  , null
  , size
  -- * Creation
  , empty
  -- * Modification
  , takeSlot
  , takeSlot_
  , takeSlots
  , takeSlots_
  , releaseSlot
  ) where

import Prelude hiding (null)
import Data.Tuple (swap)
import Data.List (mapAccumL)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

type Id = Int

-- elems hold the elements in use
-- idles contains the currently unoccupied ids (but issued in the past)
-- nexts contains the lazy list of all the future ids to issue
-- therefore, keysSet elems \cap idles = \0
data Pool a = Pool
    (IM.IntMap a) -- the busy / occupied cells
    IS.IntSet -- the idle cells
    [Id] -- the remaining ids

-- TODO: Show instance, Eq instance..?

busy :: Pool a -> IM.IntMap a
busy (Pool busy _ _) = busy

idles :: Pool a -> IS.IntSet
idles (Pool _ idles _) = idles

empty :: Pool a
empty = Pool IM.empty IS.empty [0..]

-- all the slots in the pool, those busy contain a 'Just a'; whereas the idle ones have a Nothing
slots :: Pool a -> IM.IntMap (Maybe a)
slots (Pool busy idles _) = IM.union (Just <$> busy) (IM.fromSet (const Nothing) idles)

null :: Pool a -> Bool
null = (&&) <$> IM.null . busy <*> IS.null . idles

size :: Pool a -> Int
size = (+) <$> IM.size . busy <*> IS.size . idles

takeSlot_ :: a -> Pool a -> Pool a
takeSlot_ a = snd . takeSlot a

takeSlot :: a -> Pool a -> (Id, Pool a)
takeSlot a (Pool busy idles nexts)
  | IS.null idles =
    let (k:nexts') = nexts
    in (k, Pool (IM.insert k a busy) idles nexts')
  | otherwise =
    let (k, idles') = IS.deleteFindMin idles
    in (k, Pool (IM.insert k a busy) idles' nexts)

-- insert all elements into the pool, starting from the left
takeSlots :: [a] -> Pool a -> ([Id], Pool a)
takeSlots l p = swap $ mapAccumL (\p a -> let (k, p') = takeSlot a p in (p', k)) p l

takeSlots_ :: [a] -> Pool a -> Pool a
takeSlots_ l = snd . takeSlots l

releaseSlot :: Id -> Pool a -> Pool a
releaseSlot k p@(Pool busy idles nexts)
  | IM.member k busy = Pool (IM.delete k busy) (IS.insert k idles) nexts
  | otherwise = p
